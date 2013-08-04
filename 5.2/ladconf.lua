local OPCODE = { "MOVE", "LOADK", "LOADKX", "LOADBOOL", "LOADNIL",
                 "GETUPVAL", "GETTABUP", "GETTABLE", "SETTABUP",
                 "SETUPVAL", "SETTABLE", "NEWTABLE", "SELF",
                 "ADD", "SUB", "MUL", "DIV", "MOD", "POW",
                 "UNM", "NOT", "LEN", "CONCAT", "JMP",
                 "EQ", "LT", "LE", "TEST", "TESTSET",
                 "CALL", "TAILCALL", "RETURN", "FORLOOP",
                 "FORPREP", "TFORCALL", "TFORLOOP", "SETLIST",
                 "CLOSURE", "VARARG", "EXTRAARG" }

local LUA_ENV = "_ENV"
local LUA_SOURCE = "@Lua Assembler/Disassembler"

local LUA_SIGNATURE = "\27Lua"
local LUAC_TAIL = "\x19\x93\r\n\x1a\n"
local LUAC_HEADERSIZE = string.len(LUA_SIGNATURE) + 2 + 6 + string.len(LUAC_TAIL)
local LUA_VERSION = 82
local LUA_FORMAT = 0

local ENDIANNESS = 1
local INT = 4
local SIZE_T = 8
local INSTRUCTION = 4
local LUA_NUMBER = 8
local INTEGRAL = 0

local LUA_TNIL = 0
local LUA_TBOOLEAN = 1
local LUA_TNUMBER = 3
local LUA_TSTRING = 4

local iABC = 0
local iABx = 1
local iAsBx = 2
local iAx = 3

local SIZE_C = 9
local SIZE_B = 9
local SIZE_Bx = (SIZE_C + SIZE_B)
local SIZE_A = 8
local SIZE_Ax = (SIZE_C + SIZE_B + SIZE_A)

local SIZE_OP = 6

local POS_OP = 0
local POS_A = (POS_OP + SIZE_OP)
local POS_C = (POS_A + SIZE_A)
local POS_B = (POS_C + SIZE_C)
local POS_Bx = POS_C
local POS_Ax = POS_A

local MASK_OP = math.ldexp(1, SIZE_OP)
local MASK_A = math.ldexp(1, SIZE_A)
local MASK_B = math.ldexp(1, SIZE_B)
local MASK_C = math.ldexp(1, SIZE_C)
local MASK_Bx = math.ldexp(1, SIZE_Bx)
local MASK_Ax = math.ldexp(1, SIZE_Ax)

local ABC = { SIZE_OP, SIZE_A, SIZE_C, SIZE_B }

local INT_MAX = (2 ^ ((SIZE_T * INT) - 1)) - 1
local LUAI_BITSINT
if INT_MAX-20 < 32760 then
  LUAI_BITSINT = 16
elseif INT_MAX > 2147483640 then
  LUAI_BITSINT = 32
else
  error ("you must define LUAI_BITSINT with number of bits in an integer")
end

local MAX_INT = INT_MAX - 2

local MAXARG_Bx, MAXARG_sBx, MAXARG_Ax

if SIZE_Bx < LUAI_BITSINT-1 then
  MAXARG_Bx = bit32.lshift(1,SIZE_Bx)-1
  MAXARG_sBx = bit32.rshift(MAXARG_Bx,1)
else
  MAXARG_Bx = MAX_INT
  MAXARG_sBx = MAX_INT
end

if SIZE_Ax < LUAI_BITSINT-1 then
  MAXARG_Ax = bit32.lshift(1,SIZE_Ax)-1
else
  MAXARG_Ax = MAX_INT
end

local MAXARG_A = bit32.lshift(1,SIZE_A)-1
local MAXARG_B = bit32.lshift(1,SIZE_B)-1
local MAXARG_C = bit32.lshift(1,SIZE_C)-1

local BITRK = bit32.lshift(1, (SIZE_B - 1))

local function ISK(x)
  return bit32.btest(bit32.band(x, BITRK))
end

local function INDEXK(r)
  return bit32.band(r, bit32.bnot(BITRK))
end

local MAXINDEXRK = BITRK - 1

local function RKASK(x)
  return bit32.bor(x, BITRK)
end

local function get_bit(x,a,b)
  return (math.floor((x / 2^a) % 2^b))
end

local OpArgN = 0
local OpArgU = 1
local OpArgR = 2
local OpArgK = 3

local opmodes = {
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iABC }, 	-- MOVE
  { T = 0, A = 1, B = OpArgK, C = OpArgN, mode = iABx },	-- OP_LOADK
  { T = 0, A = 1, B = OpArgN, C = OpArgN, mode = iABx },	-- OP_LOADKX
  { T = 0, A = 1, B = OpArgU, C = OpArgU, mode = iABC },	-- OP_LOADBOOL
  { T = 0, A = 1, B = OpArgU, C = OpArgN, mode = iABC },	-- OP_LOADNIL
  { T = 0, A = 1, B = OpArgU, C = OpArgN, mode = iABC },	-- OP_GETUPVAL
  { T = 0, A = 1, B = OpArgU, C = OpArgK, mode = iABC },	-- OP_GETTABUP
  { T = 0, A = 1, B = OpArgR, C = OpArgK, mode = iABC },	-- OP_GETTABLE
  { T = 0, A = 0, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_SETTABUP
  { T = 0, A = 0, B = OpArgU, C = OpArgN, mode = iABC },	-- OP_SETUPVAL
  { T = 0, A = 0, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_SETTABLE
  { T = 0, A = 1, B = OpArgU, C = OpArgU, mode = iABC },	-- OP_NEWTABLE
  { T = 0, A = 1, B = OpArgR, C = OpArgK, mode = iABC },	-- OP_SELF
  { T = 0, A = 1, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_ADD
  { T = 0, A = 1, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_SUB
  { T = 0, A = 1, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_MUL
  { T = 0, A = 1, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_DIV
  { T = 0, A = 1, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_MOD
  { T = 0, A = 1, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_POW
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iABC },	-- OP_UNM
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iABC },	-- OP_NOT
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iABC },	-- OP_LEN
  { T = 0, A = 1, B = OpArgR, C = OpArgR, mode = iABC },	-- OP_CONCAT
  { T = 0, A = 0, B = OpArgR, C = OpArgN, mode = iAsBx },	-- OP_JMP
  { T = 1, A = 0, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_EQ
  { T = 1, A = 0, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_LT
  { T = 1, A = 0, B = OpArgK, C = OpArgK, mode = iABC },	-- OP_LE
  { T = 1, A = 0, B = OpArgN, C = OpArgU, mode = iABC },	-- OP_TEST
  { T = 1, A = 1, B = OpArgR, C = OpArgU, mode = iABC },	-- OP_TESTSET
  { T = 0, A = 1, B = OpArgU, C = OpArgU, mode = iABC },	-- OP_CALL
  { T = 0, A = 1, B = OpArgU, C = OpArgU, mode = iABC },	-- OP_TAILCALL
  { T = 0, A = 0, B = OpArgU, C = OpArgN, mode = iABC },	-- OP_RETURN
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iAsBx },	-- OP_FORLOOP
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iAsBx },	-- OP_FORPREP
  { T = 0, A = 0, B = OpArgN, C = OpArgU, mode = iABC },	-- OP_TFORCALL
  { T = 0, A = 1, B = OpArgR, C = OpArgN, mode = iAsBx },	-- OP_TFORLOOP
  { T = 0, A = 0, B = OpArgU, C = OpArgU, mode = iABC },	-- OP_SETLIST
  { T = 0, A = 1, B = OpArgU, C = OpArgN, mode = iABx },	-- OP_CLOSURE
  { T = 0, A = 1, B = OpArgU, C = OpArgN, mode = iABC },	-- OP_VARARG
  { T = 0, A = 0, B = OpArgU, C = OpArgU, mode = iAx },		-- OP_EXTRAARG
}

local function get_op_mode(o)
  return opmodes[o].mode
end

local ladconf = {
  OPCODE = OPCODE,
  LUA_ENV = LUA_ENV,
  LUA_SOURCE = LUA_SOURCE,
  LUA_SIGNATURE = LUA_SIGNATURE,
  LUAC_TAIL = LUAC_TAIL,
  LUAC_HEADERSIZE = LUAC_HEADERSIZE,
  LUA_VERSION = LUA_VERSION,
  LUA_FORMAT = LUA_FORMAT,
  ENDIANNESS = ENDIANNESS,
  INT = INT,
  SIZE_T = SIZE_T,
  INSTRUCTION = INSTRUCTION,
  LUA_NUMBER = LUA_NUMBER,
  INTEGRAL = INTEGRAL,
  LUA_TNIL = LUA_TNIL,
  LUA_TBOOLEAN = LUA_TBOOLEAN,
  LUA_TNUMBER = LUA_TNUMBER,
  LUA_TSTRING = LUA_TSTRING,
  iABC = iABC,
  iABx = iABx,
  iAsBx = iAsBx,
  iAx = iAx,
  SIZE_C = SIZE_C,
  SIZE_B = SIZE_B,
  SIZE_Bx = SIZE_Bx,
  SIZE_A = SIZE_A,
  SIZE_Ax = SIZE_Ax,
  SIZE_OP = SIZE_OP,
  POS_OP = POS_OP,
  POS_A = POS_A,
  POS_C = POS_C,
  POS_B = POS_B,
  POS_Bx = POS_Bx,
  POS_Ax = POS_Ax,
  MASK_OP = MASK_OP,
  MASK_A = MASK_A,
  MASK_B = MASK_B,
  MASK_C = MASK_C,
  MASK_Bx = MASK_Bx,
  MASK_Ax = MASK_Ax,
  ABC = ABC,
  INT_MAX = INT_MAX,
  LUAI_BITSINT = LUAI_BITSINT,
  MAX_INT = MAX_INT,
  MAXARG_Bx = MAXARG_Bx,
  MAXARG_sBx = MAXARG_sBx,
  MAXARG_Ax = MAXARG_Ax,
  MAXARG_A = MAXARG_A,
  MAXARG_B = MAXARG_B,
  MAXARG_C = MAXARG_C,
  BITRK = BITRK,
  ISK = ISK,
  INDEXK = INDEXK,
  MAXINDEXRK = MAXINDEXRK,
  RKASK = RKASK,
  get_bit = get_bit,
  OpArgN = OpArgN,
  OpArgU = OpArgU,
  OpArgR = OpArgR,
  OpArgK = OpArgK,
  opmodes = opmodes,
  get_op_mode = get_op_mode,
}

return ladconf
