local ladconf = require("ladconf")

local function check_signature(bytecode)
  local sig = string.format(string.format("%o", string.byte(bytecode, 1)))
  sig = sig .. string.char(string.byte(bytecode, 2))
  sig = sig .. string.char(string.byte(bytecode, 3))
  sig = sig .. string.char(string.byte(bytecode, 4))
  sig = string.gsub(sig, "33", "\033")
  if sig ~= ladconf.LUA_SIGNATURE then
    error ("Not a Lua bytecode!")
  end
end

local function check_version(bytecode)
  if string.byte(bytecode, 5) ~= ladconf.LUA_VERSION then
    error ("Disassembler works only with Lua 5.2!")
  end
end

local function check_header(bytecode)
  check_signature(bytecode)
  check_version(bytecode)
end

local function set_auto_flags(bytecode)
  ladconf.ENDIANNESS = string.byte(bytecode, 7)
  ladconf.INT = string.byte(bytecode, 8)
  ladconf.SIZE_T = string.byte(bytecode, 9)
  ladconf.INSTRUCTION = string.byte(bytecode, 10)
  ladconf.LUA_NUMBER = string.byte(bytecode, 11)
  ladconf.INTEGRAL = string.byte(bytecode, 12)
end

local function parse_int(bytecode, k, n)
  local i = "0x"
  local s,e = k + (n-1), k + 1
  for p=s,e,-1 do
    i = i .. string.format("%02x", string.byte(bytecode, p))
  end
  i = i .. string.format("%02x", string.byte(bytecode, k))
  return tonumber(i),k + (n-1)
end

local function parse_instruction(bytecode, k)
  local code = "0x"
  local n = k + (ladconf.INSTRUCTION - 1)
  for i=n,k,-1 do
    code = code .. string.format("%02x", string.byte(bytecode, i))
  end
  return code,k + (ladconf.INSTRUCTION - 1)
end

local function parse_code(bytecode, k, parsed)
  parsed.sizecode,k = parse_int(bytecode, k, ladconf.INT)
  parsed.code = {}
  for i=1,parsed.sizecode do
    parsed.code[i],k = parse_instruction(bytecode, k + 1)
  end
  return k
end

local function setnilvalue(parsed)
  parsed.type = ladconf.LUA_TNIL
  parsed.value = 'nil'
end

local function setbvalue(parsed, boolean)
  parsed.type = ladconf.LUA_TBOOLEAN
  if boolean == 0 then
    parsed.value = 'false'
  end
  parsed.value = 'true'
end

local function setnvalue(parsed, number)
  parsed.type = ladconf.LUA_TNUMBER
  parsed.value = number
end

local function iscntrl(x)
  if (x >= 0 and x <= 31) or (x == 127) then return true end
  return false
end

local function isprint(x)
  return not iscntrl(x)
end

local function fixed_string(str)
  local new_str = ""
  for i=1,string.len(str) do
    char = string.byte(str, i)
    if char == 34 then new_str = new_str .. string.format("\\\"")
    elseif char == 92 then new_str = new_str .. string.format("\\\\")
    elseif char == 7 then new_str = new_str .. string.format("\\a")
    elseif char == 8 then new_str = new_str .. string.format("\\b")
    elseif char == 12 then new_str = new_str .. string.format("\\f")
    elseif char == 10 then new_str = new_str .. string.format("\\n")
    elseif char == 13 then new_str = new_str .. string.format("\\r")
    elseif char == 9 then new_str = new_str .. string.format("\\t")
    elseif char == 11 then new_str = new_str .. string.format("\\v")
    else
      if isprint(char) then
        new_str = new_str .. string.format("%c", char)
      else
        new_str = new_str .. string.format("\\%03d", char)
      end
    end
  end
  return new_str
end

local function setsvalue(parsed, string)
  parsed.type = ladconf.LUA_TSTRING
  parsed.value = fixed_string(string)
end

local function parse_number(bytecode, k)
  sign = 1
  mantissa = string.byte(bytecode, k+6) % 16
  for i=k+5,k,-1 do mantissa = mantissa * 256 + string.byte(bytecode, i) end
  if string.byte(bytecode,k+7) > 127 then sign = -1 end
  exponent = (string.byte(bytecode, k+7) % 128) * 16 + math.floor(string.byte(bytecode, k+6) / 16)
  if exponent == 0 then return 0 end
  mantissa = (math.ldexp(mantissa, -52) + 1) * sign
  return math.ldexp(mantissa, exponent - 1023)
end

local function parse_string(bytecode, k, len)
  local str = ""
  for i=k+1,k+(len-1) do
    local byte = string.byte(bytecode, i)
    if byte ~= 0 then
      str = str .. string.char(byte)
    else
      str = str .. '\0'
    end
  end
  return str
end

local function parse_constants(bytecode, k, parsed)
  parsed.sizek,k = parse_int(bytecode, k, ladconf.INT)
  parsed.k = {}
  for i=1,parsed.sizek do
    k = k + 1
    local t = string.byte(bytecode, k)
    parsed.k[i] = {}
    if t == ladconf.LUA_TNIL then
      setnilvalue(parsed.k[i])
    elseif t == ladconf.LUA_TBOOLEAN then
      k = k + 1
      setbvalue(parsed.k[i], string.byte(bytecode, k))
    elseif t == ladconf.LUA_TNUMBER then
      setnvalue(parsed.k[i], parse_number(bytecode, k + 1))
      k = k + ladconf.LUA_NUMBER
    elseif t == ladconf.LUA_TSTRING then
      local len
      len,k = parse_int(bytecode, k + 1, ladconf.SIZE_T)
      setsvalue(parsed.k[i], parse_string(bytecode, k, len))
      k = k + len
    end
  end
  return k
end

local function parse_upvalues(bytecode, k, parsed)
  parsed.sizeupvalues,k = parse_int(bytecode, k, ladconf.INT)
  parsed.upvalues = {}
  for i=1,parsed.sizeupvalues do
    parsed.upvalues[i] = {}
    parsed.upvalues[i].instack = string.byte(bytecode, k + 1)
    parsed.upvalues[i].idx = string.byte(bytecode, k + 2)
    k = k + 2
  end
  return k
end

local function parse_debug(bytecode, k, parsed)
  local len,n
  len,k = parse_int(bytecode, k, ladconf.SIZE_T)
  parsed.source = parse_string(bytecode, k + 1, len)
  k = k + len
  parsed.sizelineinfo,k = parse_int(bytecode, k + 1, ladconf.INT)
  parsed.lineinfo = {}
  for i=1,parsed.sizelineinfo do
    parsed.lineinfo[i],k = parse_int(bytecode, k + 1, ladconf.INT)
  end
  parsed.sizelocvars,k = parse_int(bytecode, k + 1, ladconf.INT)
  parsed.locvars = {}
  for i=1,parsed.sizelocvars do
    parsed.locvars[i] = {}
    len,k = parse_int(bytecode, k + 1, ladconf.SIZE_T)
    parsed.locvars[i].varname = parse_string(bytecode, k, len)
    k = k + len
    parsed.locvars[i].startpc,k = parse_int(bytecode, k + 1, ladconf.INT)
    parsed.locvars[i].endpc,k = parse_int(bytecode, k + 1, ladconf.INT)
  end
  n,k = parse_int(bytecode, k + 1, ladconf.INT)
  for i=1,n do
    len,k = parse_int(bytecode, k + 1, ladconf.SIZE_T)
    parsed.upvalues[i].name = parse_string(bytecode, k, len)
    k = k + len
  end
  return k
end

local function parse_function(bytecode, k, parsed)
  parsed.linedefined,k = parse_int(bytecode, k + 1, ladconf.INT)
  parsed.lastlinedefined,k = parse_int(bytecode, k + 1, ladconf.INT)
  parsed.numparams,k = string.byte(bytecode, k + 1), k + 1
  parsed.is_vararg,k = string.byte(bytecode, k + 1), k + 1
  parsed.maxstacksize,k = string.byte(bytecode, k + 1), k + 1
  k = parse_code(bytecode, k + 1, parsed)
  k = parse_constants(bytecode, k + 1, parsed)
  parsed.sizep,k = parse_int(bytecode, k + 1, ladconf.INT)
  parsed.p = {}
  for i=1,parsed.sizep do
    parsed.p[i] = {}
    parsed.p[i].parent = parsed.id
    parsed.p[i].id = i
    k = parse_function(bytecode, k, parsed.p[i])
  end
  k = parse_upvalues(bytecode, k + 1, parsed)
  k = parse_debug(bytecode, k + 1, parsed)
  return k
end

local function parse(bytecode)
  local parsed = {}
  set_auto_flags(bytecode)
  parsed.id = 0
  parse_function(bytecode, ladconf.LUAC_HEADERSIZE, parsed)
  return parsed
end

local function get_opcode(i)
  return ladconf.get_bit(i, ladconf.POS_OP, ladconf.SIZE_OP) + 1
end

local function getarg_A(i)
  return ladconf.get_bit(i, ladconf.POS_A, ladconf.SIZE_A)
end

local function getarg_B(i)
  return ladconf.get_bit(i, ladconf.POS_B, ladconf.SIZE_B)
end

local function getarg_C(i)
  return ladconf.get_bit(i, ladconf.POS_C, ladconf.SIZE_C)
end

local function getarg_Bx(i)
  return ladconf.get_bit(i, ladconf.POS_Bx, ladconf.SIZE_Bx)
end

local function getarg_Ax(i)
  return ladconf.get_bit(i, ladconf.POS_Ax, ladconf.SIZE_Ax)
end

local function getarg_sBx(i)
  return getarg_Bx(i) - ladconf.MAXARG_sBx
end

local function get_b_mode(o)
  return ladconf.opmodes[o].B
end

local function get_c_mode(o)
  return ladconf.opmodes[o].C
end

local function ISK(x)
  return ladconf.ISK(x)
end

local function INDEXK(x)
  return ladconf.INDEXK(x)
end

local function MYK(x)
  return (-1-(x))
end

local function UPVALNAME(parsed, x)
  if parsed.upvalues[x].name then
    return parsed.upvalues[x].name
  end
  return "-"
end

local function get_funcline(parsed, pc)
  if parsed.sizelineinfo > 0 then
    return parsed.lineinfo[pc]
  end
  return 0
end

local function print_header(parsed)
  local function SS(x) if x == 1 then return "" else return "s" end end
  local function S(x) return x,SS(x) end

  local func_name = "function"
  local is_vararg = ""

  if parsed.linedefined == 0 and parsed.id == 0 then
    func_name = "main"
  end
  if parsed.is_vararg ~= 0 then
    is_vararg = "+"
  end

  io.write(string.format("\n%s <%s:%d,%d> (%d instruction%s at %d)\n",
                  func_name, parsed.source,
                  parsed.linedefined, parsed.lastlinedefined,
                  parsed.sizecode, SS(parsed.sizecode), 0))
  io.write(string.format("%d%s param%s, %d slot%s, %d upvalue%s, ",
                  parsed.numparams, is_vararg, SS(parsed.numparams),
                  parsed.maxstacksize, SS(parsed.maxstacksize),
                  parsed.sizeupvalues, SS(parsed.sizeupvalues)))
  io.write(string.format("%d local%s, %d constant%s, %d function%s\n",
                  parsed.sizelocvars, SS(parsed.sizelocvars),
                  parsed.sizek, SS(parsed.sizek),
                  parsed.sizep, SS(parsed.sizep)))
end

local function print_string(str)
  io.write(string.format('"%s"', str))
end

local function print_constant(parsed, i)
  if not parsed.k[i] then
    io.write("nil")
    return
  end
  local t = parsed.k[i].type
  local v = parsed.k[i].value
  if t == ladconf.LUA_TNIL or
     t == ladconf.LUA_TBOOLEAN or
     t == ladconf.LUA_TNUMBER then
    io.write(string.format("%s", v))
  elseif t == ladconf.LUA_TSTRING then
    print_string(v)
  end
end

local function print_code(parsed)
  local n = parsed.sizecode
  local pc = 1
  while pc <= n do
    local i = parsed.code[pc]
    local o = get_opcode(i)
    local a = getarg_A(i)
    local b = getarg_B(i)
    local c = getarg_C(i)
    local ax = getarg_Ax(i)
    local bx = getarg_Bx(i)
    local sbx = getarg_sBx(i)
    local line = get_funcline(parsed, pc)
    local opcode = ladconf.OPCODE[o]

    io.write(string.format("\t%d\t", pc))
    if line > 0 then
      io.write(string.format("[%d]\t", line))
    else
      io.write(string.format("[-]\t"))
    end
    io.write(string.format("%-9s\t", opcode))

    local opmode = ladconf.get_op_mode(o)
    if opmode == ladconf.iABC then
      io.write(string.format("%d", a))
      local bmode = get_b_mode(o)
      local cmode = get_c_mode(o)
      if bmode ~= ladconf.OpArgN then
        if ISK(b) then
          io.write(string.format(" %d", MYK(INDEXK(b))))
        else
          io.write(string.format(" %d", b))
        end
      end
      if cmode ~= ladconf.OpArgN then
        if ISK(c) then
          io.write(string.format(" %d", MYK(INDEXK(c))))
        else
          io.write(string.format(" %d", c))
        end
      end
    elseif opmode == ladconf.iABx then
      io.write(string.format("%d", a))
      local bmode = get_b_mode(o)
      if bmode == ladconf.OpArgK then
        io.write(string.format(" %d", MYK(bx)))
      end
      if bmode == ladconf.OpArgU then
        io.write(string.format(" %d", bx))
      end
    elseif opmode == ladconf.iAsBx then
      io.write(string.format("%d %d", a, sbx))
    elseif opmode == ladconf.iAx then
      io.write(string.format("%d", MYK(ax)))
    end

    if opcode == "LOADK" then
      io.write(string.format("\t; "))
      print_constant(parsed, bx+1)
    elseif opcode == "GETUPVAL" or
           opcode == "SETUPVAL" then
      io.write(string.format("\t; %s", UPVALNAME(parsed, b+1)))
    elseif opcode == "GETTABUP" then
      io.write(string.format("\t; %s", UPVALNAME(parsed, b+1)))
      if ISK(c) then
        io.write(string.format(" "))
        print_constant(parsed, INDEXK(c+1))
      end
    elseif opcode == "SETTABUP" then
      io.write(string.format("\t; %s", UPVALNAME(parsed, a+1)))
      if ISK(b) then
        io.write(string.format(" "))
        print_constant(parsed, INDEXK(b+1))
      end
      if ISK(c) then
        io.write(string.format(" "))
        print_constant(parsed, INDEXK(c+1))
      end
    elseif opcode == "GETTABLE" or
           opcode == "SELF" then
      if ISK(c) then
        io.write(string.format("\t; "))
        print_constant(parsed, INDEXK(c+1))
      end
    elseif opcode == "SETTABLE" or
           opcode == "ADD" or
           opcode == "SUB" or
           opcode == "MUL" or
           opcode == "DIV" or
           opcode == "POW" or
           opcode == "EQ" or
           opcode == "LT" or
           opcode == "LE" then
      if ISK(b) or ISK(c) then
        io.write(string.format("\t; "))
        if ISK(b) then
          print_constant(parsed, INDEXK(b+1))
        else
          io.write(string.format("-"))
        end
        io.write(string.format(" "))
        if ISK(c) then
          print_constant(parsed, INDEXK(c+1))
        else
          io.write(string.format("-"))
        end
      end
    elseif opcode == "JMP" or
           opcode == "FORLOOP" or
           opcode == "FORPREP" or
           opcode == "TFORLOOP" then
      io.write(string.format("\t; to %d", sbx+pc+1))
    elseif opcode == "CLOSURE" then
      io.write(string.format("\t; 0x"))
    elseif opcode == "SETLIST" then
      if c == 0 then
        pc = pc + 1
        io.write(string.format("\t; %d", parsed.code[pc]))
      else
        io.write(string.format("\t; %d", c))
      end
    elseif opcode == "EXTRAARG" then
      io.write(string.format("\t; "))
      print_constant(parsed, ax+1)
    end
    io.write(string.format("\n"))
    pc = pc + 1
  end
end

local function print_debug(parsed)
  local n
  n = parsed.sizek
  io.write(string.format("constants (%d) for 0:\n", n))
  for i=1,n do
    io.write(string.format("\t%d\t", i))
    print_constant(parsed, i)
    io.write(string.format("\n"))
  end
  n = parsed.sizelocvars
  io.write(string.format("locals (%d) for 0:\n", n))
  for i=1,n do
    io.write(string.format("\t%d\t%s\t%d\t%d\n", i-1,
                           parsed.locvars[i].varname,
                           parsed.locvars[i].startpc+1,
                           parsed.locvars[i].endpc+1))
  end
  n = parsed.sizeupvalues
  io.write(string.format("upvalues (%d) for 0:\n", n))
  for i=1,n do
    io.write(string.format("\t%d\t%s\t%d\t%d\n", i-1,
                           UPVALNAME(parsed, i),
                           parsed.upvalues[i].instack,
                           parsed.upvalues[i].idx))
  end
end

local function print_function(parsed, full)
  local n = parsed.sizep
  print_header(parsed)
  print_code(parsed)
  if full then print_debug(parsed) end
  for i=1,n do
    print_function(parsed.p[i], full)
  end
end

local function write_header(output, parsed)
  local fname
  local np = parsed.numparams
  if parsed.linedefined == 0 and parsed.id == 0 then
    fname = "main"
  else
    fname = string.format("F_%s_%s", parsed.parent, parsed.id)
  end
  output:write(string.format("\nfunction %s(%s):\n", fname, np))
end

local function write_par(output, x)
  output:write(string.format("%s", x))
end

local function write_sep(output)
  output:write(string.format(", "))
end

local function R(x)
  return "$" .. x
end

local function UpValue(parsed, x)
  return parsed.upvalues[x+1].name
end

local function KST(parsed, x)
  return parsed.k[x+1].value
end

local function Kst(parsed, x)
  local t = parsed.k[x+1].type
  local v = parsed.k[x+1].value
  if t == ladconf.LUA_TNIL or
     t == ladconf.LUA_TBOOLEAN or
     t == ladconf.LUA_TNUMBER then
    return v
  elseif t == ladconf.LUA_TSTRING then
    return '"' .. v .. '"'
  end
end

local function RKST(parsed, x)
  if ISK(x) then
    return KST(parsed, INDEXK(x))
  else
    return R(x)
  end
end

local function RK(parsed, x)
  if ISK(x) then
    return Kst(parsed, INDEXK(x))
  else
    return R(x)
  end
end

local function KPROTO(parsed, x)
  local parent = parsed.p[x+1].parent
  local id = parsed.p[x+1].id
  return string.format("F_%s_%s", parent, id)
end

local function write_code(output, parsed)
  local n = parsed.sizecode
  for pc=1,n do
    local i = parsed.code[pc]
    local o = get_opcode(i)
    local a = getarg_A(i)
    local b = getarg_B(i)
    local c = getarg_C(i)
    local ax = getarg_Ax(i)
    local bx = getarg_Bx(i)
    local sbx = getarg_sBx(i)
    local line = get_funcline(parsed, pc)
    local opcode = ladconf.OPCODE[o]

    output:write(string.format("\t%d\t", pc))
    output:write(string.format("[%d]\t", line))
    output:write(string.format("%-9s\t", opcode))

    if opcode == "MOVE" or
       opcode == "UNM" or
       opcode == "NOT" or
       opcode == "LEN" then
      -- R(A) R(B)
     write_par(output, R(a))
     write_sep(output)
     write_par(output, R(b))
    elseif opcode == "LOADK" then
      -- R(A) Kst(Bx)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, Kst(parsed, bx))
    elseif opcode == "LOADKX" then
      -- R(A)
      write_par(output, R(a))
    elseif opcode == "LOADBOOL" or
           opcode == "NEWTABLE" or
           opcode == "CALL" or
           opcode == "TAILCALL" or
           opcode == "SETLIST" then
      -- R(A) B C
      write_par(output, R(a))
      write_sep(output)
      write_par(output, b)
      write_sep(output)
      write_par(output, c)
    elseif opcode == "LOADNIL" or
           opcode == "RETURN" or
           opcode == "VARARG" then
      -- R(A) B
      write_par(output, R(a))
      write_sep(output)
      write_par(output, b)
    elseif opcode == "GETUPVAL" or
           opcode == "SETUPVAL" then
      -- R(A) UpValue(B)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, UpValue(parsed, b))
    elseif opcode == "GETTABUP" then
      -- R(A) UpValue(B) RK(C)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, UpValue(parsed, b))
      write_sep(output)
      write_par(output, RK(parsed, c))
    elseif opcode == "GETTABLE" or
           opcode == "SELF" then
      -- R(A) R(B) RK(C)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, R(b))
      write_sep(output)
      write_par(output, RK(parsed, c))
    elseif opcode == "SETTABUP" then
      -- UpValue(A) RK(B) RK(C)
      write_par(output, UpValue(parsed, a))
      write_sep(output)
      write_par(output, RK(parsed, b))
      write_sep(output)
      write_par(output, RK(parsed, c))
    elseif opcode == "SETTABLE" or
           opcode == "ADD" or
           opcode == "SUB" or
           opcode == "MUL" or
           opcode == "DIV" or
           opcode == "MOD" or
           opcode == "POW" then
      -- R(A) RK(B) RK(C)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, RK(parsed, b))
      write_sep(output)
      write_par(output, RK(parsed, c))
    elseif opcode == "CONCAT" then
      -- R(A) R(B) R(C)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, R(b))
      write_sep(output)
      write_par(output, R(c))
    elseif opcode == "JMP" then
      -- A sBx
      write_par(output, a)
      write_sep(output)
      write_par(output, sbx + pc + 1)
    elseif opcode == "EQ" or
           opcode == "LT" or
           opcode == "LE" then
      -- A RK(B) RK(C)
      write_par(output, a)
      write_sep(output)
      write_par(output, RK(parsed, b))
      write_sep(output)
      write_par(output, RK(parsed, c))
    elseif opcode == "TEST" or
           opcode == "TFORCALL" then
      -- R(A) C
      write_par(output, R(a))
      write_sep(output)
      write_par(output, c)
    elseif opcode == "TESTSET" then
      -- R(A) R(B) C
      write_par(output, R(a))
      write_sep(output)
      write_par(output, R(b))
      write_sep(output)
      write_par(output, c)
    elseif opcode == "FORLOOP" or
           opcode == "FORPREP" or
           opcode == "TFORLOOP" then
      -- R(A) sBx
      write_par(output, R(a))
      write_sep(output)
      write_par(output, sbx + pc + 1)
    elseif opcode == "CLOSURE" then
      -- R(A) KPROTO(Bx)
      write_par(output, R(a))
      write_sep(output)
      write_par(output, KPROTO(parsed, bx))
    elseif opcode == "EXTRAARG" then
      -- Ax
      po = get_opcode(parsed.code[pc-1])
      popcode = ladconf.OPCODE[po]
      if popcode == "LOADKX" then
        write_par(output, Kst(parsed, ax))
      elseif popcode == "SETLIST" then
        write_par(output, ax)
      end
    end
    output:write(string.format("\n"))
  end
end

local function write_function(output, parsed)
  local n = parsed.sizep
  write_header(output, parsed)
  write_code(output, parsed)
  for i=1,n do
    write_function(output, parsed.p[i])
  end
end

local function write(filename, parsed)
  local output = assert(io.open(filename, "w"))
  write_function(output, parsed)
  output:close()
end

local disassembler = {
  parse = parse,
  write = write,
  print_function = print_function,
}

return disassembler
