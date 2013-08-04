local re = require("re")
local ladconf = require("ladconf")

local OPCODE = { MOVE = 0, LOADK = 1, LOADKX = 2, LOADBOOL = 3, LOADNIL = 4,
                 GETUPVAL = 5, GETTABUP = 6, GETTABLE = 7, SETTABUP = 8,
                 SETUPVAL = 9, SETTABLE = 10, NEWTABLE = 11, SELF = 12,
                 ADD = 13, SUB = 14, MUL = 15, DIV = 16, MOD = 17, POW = 18,
                 UNM = 19, NOT = 20, LEN = 21, CONCAT = 22, JMP = 23,
                 EQ = 24, LT = 25, LE = 26, TEST = 27, TESTSET = 28,
                 CALL = 29, TAILCALL = 30, RETURN = 31, FORLOOP = 32,
                 FORPREP = 33, TFORCALL = 34, TFORLOOP = 35, SETLIST = 36,
                 CLOSURE = 37, VARARG = 38, EXTRAARG = 39 }

local grammar = [[
  prog <- s ( {:tag: '' -> 'prog':} function )* -> {} !.
  function <- ( {:tag: '' -> 'func':} {:header: header:} {:codelist: codelist:}) -> {}
  header <- ("function" s {:name: name:} s "(" s {:numparams: n:} s ")" s ":" s) -> {}
  codelist <- code+ -> {}
  code <- autocode / manualcode
  autocode <- ( {:tag: '' -> 'code':} n s ln s op s param s ("," s param s)* ) -> {} 
  manualcode <- ( {:tag: '' -> 'code':} ({:label: label:} s )? op s param s ("," s param s)* ) -> {} 
  n <- %d+
  label <- name ":"
  name <- ( !reserved {[a-zA-Z_][a-zA-Z0-9_]*} )
  ln <- "[" %d+ "]"
  op <- {:op: !reserved %a+ -> to_upper:}
  param <- register / number / string
  register <- ( {:tag: '' -> 'reg':} ("$" {n}) -> to_number ) -> {}
  number <- ( {:tag: '' -> 'num':} ( hex / float / int ) ) -> {}
  string <- ( {:tag: '' -> 'str':} ( name / shortstr ) ) -> {}
  shortstr <- ( '"' {('\\' / '\"' / !'"' .)*} '"' / "'" {("\\" / "\'" / !"'" .)*} "'" ) -> to_string
  hex <- ( {:tag: '' -> 'int':} {"-"? "0" [xX] %x+} -> to_number ) -> {}
  float <- ( {:tag: '' -> 'float':} {"-"? ( (%d+ "." %d* / "." %d+) e? / %d+ e )} -> to_number ) -> {}
  e <- [eE] [+-]? n
  int <- ( {:tag: '' -> 'int':} {"-"? n} -> to_number ) -> {}
  s <- (space / comment)*
  space <- %s+
  comment <- ";" (!%nl .)*
  reserved <- "function"
]]

local function fixed_string(s)
  s = string.gsub(s, "\\\"", '\"')
  s = string.gsub(s, "\\\\", '\\')
  s = string.gsub(s, "\\a", '\a')
  s = string.gsub(s, "\\b", '\b')
  s = string.gsub(s, "\\f", '\f')
  s = string.gsub(s, "\\n", '\n')
  s = string.gsub(s, "\\r", '\r')
  s = string.gsub(s, "\\t", '\t')
  s = string.gsub(s, "\\v", '\v')
  s = string.gsub(s, "\\(%d+)", function (s) return string.char (tonumber(s)) end )
  return s
end

local defs = {
  to_number = function (n) return tonumber(n) end,
  to_string = function (s) return fixed_string(s) end,
  to_upper = function (s) return string.upper(s) end,
}

local parser = re.compile(grammar, defs)

local function parse(contents)
  return parser:match(contents)
end

local function print_header(t)
  io.write(string.format("\nfunction %s(%s):\n", t.name, t.numparams))
end

local function print_param(t)
  if t.tag == 'reg' then
    io.write(string.format("$%d", t[1]))
  elseif t.tag == 'num' then
    io.write(string.format("%s", tostring(t[1])))
  elseif t.tag == 'str' then
    io.write(string.format("%s", t[1]))
  end
end

local function print_sep()
  io.write(string.format(", "))
end

local function print_code(t, i)
  if t.tag == 'code' then
    if t.label then io.write(string.format("%s:", t.label)) end
    io.write(string.format("\t%d", i))
    io.write(string.format("\t%-9s\t", t.op))
    for i=1,#t-1 do
      print_param(t[i])
      print_sep()
    end
    print_param(t[#t])
    io.write(string.format("\n"))
  end
end

local function print_codelist(t)
  for i=1,#t do
    print_code(t[i], i)
  end
end

local function print_func(t)
  if t.tag == 'func' then
    print_header(t.header)
    print_codelist(t.codelist)
  end
end

local function print_ast(t)
  if t.tag == 'prog' then
    for k,v in ipairs(t) do
      print_func(v)
    end
  end
end

local function converge(ast)
  if ast.tag ~= 'prog' then return nil end
  local t = {}
  for k,v in ipairs(ast) do
    if v.tag ~= 'func' then return nil end
    local name = v.header.name
    t[name] = {}
    t[name].id = k
  end
  return t
end

local function R(t, f)
  if t.tag ~= 'reg' then error("Register expected") end
  local r = t[1]
  if not f.register[r] then
    f.register[r] = r
    f.maxstacksize = f.maxstacksize + 1
  end
  return r
end

local function Int(t)
  if t.tag ~= 'num' or t[1].tag ~= 'int' then
    error("Integer expected")
  end
  local i = t[1][1]
  if i < 0 then error("Positive integer expected") end
  return i
end

local function Label(t, f)
  local l = t[1]
  if not f.label[l] then error(string.format("Label %s not defined", l)) end
  return f.label[l]
end

local function Kst(t, f)
  local k, k_type

  if t.tag == 'num' then
    k = t[1][1]
    k_type = ladconf.LUA_TNUMBER
  elseif t.tag == 'str' then
    k = t[1]
    k_type = ladconf.LUA_TSTRING
  else
    error ("Constant not implemented")
  end

  if not f.const[k] then
    local n = f.sizek + 1
    f.sizek = n
    f.const[k] = n
    f.k[n] = {}
    f.k[n].k_type = k_type
    f.k[n].value = k
  end

  return f.const[k]
end

local function KPROTO(t, f, a)
  if t.tag ~= 'str' then error("Function name expected") end

  local name = t[1]

  if not a[name] then
    error(string.format("Function %s not defined", name))
  end

  if not f.func[name] then
    local n = f.sizep + 1
    f.sizep = n
    f.func[name] = n
    f.p[n] = {}
    f.p[n] = name
  end

  return f.func[name]
end

local function UpValue(t, f)
  if t.tag ~= 'str' then error("String/Name expected") end

  local name = t[1]

  if not f.upval[name] then
    local n = f.sizeupvalues + 1
    f.sizeupvalues = n
    f.upval[name] = n
    f.upvalues[n] = {}
    if name == "_ENV" then
      f.upvalues[n].instack = 0
    else
      f.upvalues[n].instack = 1
    end
    f.upvalues[n].idx = 0
    f.upvalues[n].name = name
  end

  return f.upval[name]
end

local function RK(t, f)
  if t.tag == 'reg' then
    return R(t, f)
  end
  -- TODO: fix it (does not work if sizek > 2^8)
  local k = Kst(t, f)
  if k < ladconf.MAXINDEXRK then
    return k + ladconf.MAXINDEXRK
  end
  return k
end

local function SBX(t, f, n)
  if t.tag == 'num' then
    return Int(t) - n
  elseif t.tag == 'str' then
    return Label(t, f) - n
  end
end

local function sew_label(i, t, n)
  if i.label then
    local l = i.label
    if not t[l] then t[l] = n
    else error (string.format("Label %s already defined\n", l))
    end
  end
end

local function sew_code(t, ast, all)
  local codelist = ast[t.id].codelist
  t.sizecode = #codelist
  -- first check for labels
  t.label = {}
  for k,v in ipairs(codelist) do
    sew_label(v, t.label, k)
  end
  -- after that check code
  t.code = {}
  for k,v in ipairs(codelist) do
    if v.tag ~= 'code' then return nil end
    local f,i = t,{}
    local op = v.op
    t.code[k] = i
    i.O = OPCODE[op]
    if op == "MOVE" or
       op == "UNM" or
       op == "NOT" or
       op == "LEN" then
      -- R(A) R(B)
      i.A = R(v[1], f)
      i.B = R(v[2], f)
      i.C = 0
    elseif op == "LOADK" then
      -- R(A) Kst(Bx)
      i.A = R(v[1], f)
      i.Bx = Kst(v[2], f)
    elseif op == "LOADKX" then
      -- R(A)
      if codelist[k+1].op ~= "EXTRAARG" then
        error("For LOADKX next instruction is always EXTRAARG")
      end
      i.A = R(v[1], f)
      i.Bx = 0
    elseif op == "LOADBOOL" or
	   op == "NEWTABLE" or
	   op == "CALL" or
	   op == "TAILCALL" or
	   op == "SETLIST" then
      -- R(A) B C
      i.A = R(v[1], f)
      i.B = Int(v[2])
      i.C = Int(v[3])
    elseif op == "LOADNIL" or
	   op == "RETURN" then
      -- R(A) B
      i.A = R(v[1], f)
      i.B = Int(v[2])
      i.C = 0
    elseif op == "GETUPVAL" or
	   op == "SETUPVAL" then
      -- R(A) UpValue(B)
      i.A = R(v[1], f)
      i.B = UpValue(v[2], f) - 1
      i.C = 0
    elseif op == "GETTABUP" then
      -- R(A) UpValue(B) RK(C)
      i.A = R(v[1], f)
      i.B = UpValue(v[2], f) - 1
      i.C = RK(v[3], f)
    elseif op == "GETTABLE" or
	   op == "SELF" then
      -- R(A) R(B) RK(C)
      i.A = R(v[1], f)
      i.B = R(v[2], f)
      i.C = RK(v[3], f)
    elseif op == "SETTABUP" then
      -- UpValue(A) RK(B) RK(C)
      i.A = UpValue(v[1], f) - 1
      i.B = RK(v[2], f)
      i.C = RK(v[3], f)
    elseif op == "SETTABLE" or
	   op == "ADD" or
	   op == "SUB" or
	   op == "MUL" or
	   op == "DIV" or
	   op == "MOD" or
	   op == "POW" then
      -- R(A) RK(B) RK(C)
      i.A = R(v[1], f)
      i.B = RK(v[2], f)
      i.C = RK(v[3], f)
    elseif op == "CONCAT" then
      -- R(A) R(B) R(C)
      i.A = R(v[1], f)
      i.B = R(v[2], f)
      i.C = R(v[3], f)
    elseif op == "JMP" then
      -- A sBx
      i.A = Int(v[1])
      i.sBx = SBX(v[2], f, k)
    elseif op == "EQ" or
	   op == "LT" or
	   op == "LE" then
      -- A RK(B) RK(C)
      i.A = Int(v[1])
      i.B = RK(v[2], f)
      i.C = RK(v[3], f)
    elseif op == "TEST" or
	   op == "TFORCALL" then
      -- R(A) C
      i.A = R(v[1], f)
      i.B = 0
      i.C = Int(v[2])
    elseif op == "TESTSET" then
      -- R(A) R(B) C
      i.A = R(v[1], f)
      i.B = R(v[2], f)
      i.C = Int(v[3])
    elseif op == "FORLOOP" or
	   op == "FORPREP" or
	   op == "TFORLOOP" then
      -- R(A) sBx
      i.A = R(v[1], f)
      i.sBx = SBX(v[2], f, k)
    elseif op == "CLOSURE" then
      -- R(A) KPROTO(Bx)
      i.A = R(v[1], f)
      i.Bx = KPROTO(v[2], f, all)
    elseif op == "VARARG" then
      -- R(A) B
      i.A = R(v[1], f)
      i.B = Int(v[2])
      i.C = 0
      f.is_vararg = 1
      f.numparams = i.B
    elseif op == "EXTRAARG" then
      -- Ax
      local pop = codelist[k-1].op
      if pop == "LOADKX" then
	i.Ax = Kst(v[1], f) - 1
      elseif pop == "SETLIST" then
	i.Ax = Int(v[1])
      else
	error ("EXTRAARG not expected")
      end
    else
      local str = string.format("%s not implemented\n", op)
      error (str)
    end
  end
end

local function sew_function(t, ast, all)
  local name = ast[t.id].header.name
  t.linedefined = 0
  t.lastlinedefined = 0
  t.numparams = ast[t.id].header.numparams
  if name ~= "main" then
    t.is_vararg = 0
  end
  -- registers 0/1 are always valid
  t.register = {}
  t.register[0] = 0
  t.register[1] = 1
  t.maxstacksize = 2
  -- constants
  t.sizek = 0
  t.k = {}
  t.const = {}
  -- functions
  t.sizep = 0
  t.p = {}
  t.func = {}
  -- upvalues
  if name ~= "main" then
    t.sizeupvalues = 0
    t.upvalues = {}
    t.upval = {}
  end
  t.sizelineinfo = 0
  t.sizelocvars = 0
  sew_code(t, ast, all)
end

local function sew(t, ast)
  if not t.main then error ("main not defined") end
  -- main function always have upvalue _ENV defined
  t.main.sizeupvalues = 1
  t.main.upval = {}
  t.main.upval[ladconf.LUA_ENV] = 1
  t.main.upvalues = {}
  t.main.upvalues[1] = {}
  t.main.upvalues[1].instack = 1
  t.main.upvalues[1].idx = 0
  t.main.upvalues[1].name = ladconf.LUA_ENV
  -- main is always vararg
  t.main.is_vararg = 1
  for k,v in pairs(t) do
    sew_function(v, ast, t)
  end
end

local function traverse(ast)
  local t = converge(ast)
  if not t then error("Empty File") end
  sew(t, ast)
  return t
end

local function write_byte(output, byte)
  if byte ~= 0 then
    output:write(string.format("%c", byte))
  else
    output:write('\0')
  end
end

local function get_hex(n)
  return string.format("0x%x", n)
end

local function get_int(i, s)
  local x = get_hex(i)
  local a,b = 0,8
  local t = {}
  for k=1,s do
    t[k] = ladconf.get_bit(x, a, b)
    a = a + 8
    b = b + 8
  end
  return t
end

local function write_int(output, n, s)
  local i = get_int(n, s)
  for k=1,s do
    write_byte(output, i[k])
  end
end

local function write_string(output, str)
  local len = string.len(str)
  write_int(output, len + 1, ladconf.SIZE_T)
  for i=1,len do
    write_byte(output, string.byte(str, i))
  end
  write_byte(output, 0)
end

local function write_source_name(output, name)
  write_string(output, name)
end

local function get_byte(v)
  return math.floor(v / 256), string.char(math.floor(v) % 256)
end

local function convert_number(x)
  local sign = 0
  if x < 0 then sign = 1; x = -x end
  local mantissa, exponent = math.frexp(x)
  if x == 0 then -- zero
    mantissa, exponent = 0, 0
  else
    mantissa = (mantissa * 2 - 1) * math.ldexp(0.5, 53)
    exponent = exponent + 1022
  end
  local v, byte = {}, "" -- convert to bytes
  x = mantissa
  for i = 1,6 do
    x, byte = get_byte(x); v[i] = string.byte(byte)
  end
  x, byte = get_byte(exponent * 16 + x); v[7] = string.byte(byte)
  x, byte = get_byte(sign * 128 + x); v[8] = string.byte(byte)
  return v
end

local function write_number(output, n)
  local t = convert_number(n)
  for k=1,ladconf.LUA_NUMBER do write_byte(output, t[k]) end
end

local function write_header(output)
  for i=1,string.len(ladconf.LUA_SIGNATURE) do
    write_byte(output, string.byte(ladconf.LUA_SIGNATURE, i))
  end
  write_byte(output, ladconf.LUA_VERSION)
  write_byte(output, ladconf.LUA_FORMAT)
  write_byte(output, ladconf.ENDIANNESS)
  write_byte(output, ladconf.INT)
  write_byte(output, ladconf.SIZE_T)
  write_byte(output, ladconf.INSTRUCTION)
  write_byte(output, ladconf.LUA_NUMBER)
  write_byte(output, ladconf.INTEGRAL)
  for i=1,string.len(ladconf.LUAC_TAIL) do
    write_byte(output, string.byte(ladconf.LUAC_TAIL, i))
  end
end

local function write_function_values(output, f)
  write_byte(output, f.numparams)
  write_byte(output, f.is_vararg)
  write_byte(output, f.maxstacksize)
end

local function gen_iABC(O, A, B, C)
  local field = {O, A, C, B}
  local v, i = {}, 0
  local cValue, cBits, cPos = 0, 0, 1
  -- encode an instruction
  while i < ladconf.INSTRUCTION do
    -- if need more bits, suck in a field at a time
    while cBits < 8 do
      cValue = field[cPos] * math.ldexp(1, cBits) + cValue
      cBits = cBits + ladconf.ABC[cPos]; cPos = cPos + 1
    end
    -- extract bytes to instruction string
    while cBits >= 8 do
      v[i+1] = (cValue % 256)
      cValue = math.floor(cValue / 256)
      cBits = cBits - 8; i = i + 1
    end
  end
  return v
end

local function gen_iABx(O, A, Bx)
  return gen_iABC(O, A, math.floor(Bx / ladconf.MASK_C), (Bx % ladconf.MASK_C))
end

local function gen_iAsBx(O, A, sBx)
  return gen_iABx(O, A, (sBx + ladconf.MAXARG_sBx))
end

local function gen_iAx(O, Ax)
  return gen_iABC(O, Ax, 0, 0)
end

local function write_instruction(output, i)
  local m = ladconf.get_op_mode(i.O + 1)
  local t = {}

  if m == ladconf.iABC then
    t = gen_iABC(i.O, i.A, i.B, i.C)
  elseif m == ladconf.iABx then
    t = gen_iABx(i.O, i.A, i.Bx - 1)
  elseif m == ladconf.iAsBx then
    t = gen_iAsBx(i.O, i.A, i.sBx - 1)
  elseif m == ladconf.iAx then
    t = gen_iAx(i.O, i.Ax)
  end

  for k=1,ladconf.INSTRUCTION do write_byte(output, t[k]) end
end

local function write_code(output, f)
  local n = f.sizecode
  write_int(output, n, ladconf.INT)
  for i=1,n do
    write_instruction(output, f.code[i])
  end
end

local function write_constant(output, k)
  local t = k.k_type
  write_byte(output, t)
  if t == ladconf.LUA_TNIL then
    -- do not need to write anything
  elseif t == ladconf.LUA_TBOOLEAN then
  elseif t == ladconf.LUA_TNUMBER then
    write_number(output, k.value)
  elseif t == ladconf.LUA_TSTRING then
    write_string(output, k.value)
  end
end

local function write_constants(output, f)
  local n
  n = f.sizek
  write_int(output, n, ladconf.INT)
  for i=1,n do
    write_constant(output, f.k[i])
  end
end

local function write_upvalues(output, f)
  local n = f.sizeupvalues
  write_int(output, n, ladconf.INT)
  for i=1,n do
    write_byte(output, f.upvalues[i].instack)
    write_byte(output, f.upvalues[i].idx)
  end
end

local function write_debug(output, f)
  write_string(output, ladconf.LUA_SOURCE)
  write_int(output, f.sizelineinfo, ladconf.INT)
  write_int(output, f.sizelocvars, ladconf.INT)
  local n = f.sizeupvalues
  write_int(output, n, ladconf.INT)
  for k=1,n do
    write_string(output, f.upvalues[k].name)
  end
end

local function write_function(output, current, parsed)
  write_int(output, current.linedefined, ladconf.INT) -- line defined
  write_int(output, current.lastlinedefined, ladconf.INT) -- last line defined
  write_function_values(output, current)
  write_code(output, current)
  write_constants(output, current, parsed)
  n = current.sizep
  write_int(output, n, ladconf.INT)
  for i=1,n do
    local name = current.p[i]
    write_function(output, parsed[name], parsed)
  end
  write_upvalues(output, current)
  write_debug(output, current)
end

local function write_bytecode(output, parsed)
  write_header(output)
  write_function(output, parsed.main, parsed)
end

local function write(filename, parsed)
  local output = assert(io.open(filename, "wb"))
  write_bytecode(output, parsed)
  output:close()
end

local assembler = {
  parse = parse,
  traverse = traverse,
  write = write,
  print_ast = print_ast,
}

return assembler
