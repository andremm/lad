--[[
Lua Assembler
Version 0.1
Author: Andre Murbach Maidl
]]

-- requires LPeg re module
require "re"

-- usage message
USAGE = [[
usage: %s [options] [filename]
Available options are:
-h	print this help
-b	big endian (default is little endian)
-o name output to file name (default is %s)
]]

-- global settings
config = {}
config.SIGNATURE   = "\27Lua"
config.VERSION     = 81
config.FORMAT      = 0
config.ENDIANNESS  = 1
config.INT         = 4
config.SIZE_T      = 8
config.INSTRUCTION = 4
config.LUA_NUMBER  = 8
config.INTEGRAL    = 0
config.SIZE_C      = 9
config.SIZE_B      = 9
config.SIZE_Bx     = config.SIZE_C + config.SIZE_B
config.SIZE_A      = 8
config.SIZE_OP     = 6
config.POS_OP      = 0
config.POS_A       = config.POS_OP + config.SIZE_OP
config.POS_C       = config.POS_A + config.SIZE_A
config.POS_B       = config.POS_C + config.SIZE_C
config.POS_Bx      = config.POS_C
config.iABC 	   = { config.SIZE_OP, config.SIZE_A, config.SIZE_C,  config.SIZE_B }
config.MASK_OP 	   = math.ldexp(1, config.SIZE_OP)
config.MASK_A      = math.ldexp(1, config.SIZE_A)
config.MASK_B      = math.ldexp(1, config.SIZE_B)
config.MASK_C      = math.ldexp(1, config.SIZE_C)
config.MASK_Bx     = math.ldexp(1, config.SIZE_Bx)
config.MAXARG_sBx  = math.floor((config.MASK_Bx - 1) / 2)
config.BITRK       = math.ldexp(1, config.SIZE_B - 1)

-- global types
types = {}
types.LUA_TNIL     = 0
types.LUA_TBOOLEAN = 1
types.LUA_TNUMBER  = 3
types.LUA_TSTRING  = 4

-- opcodes
opcode = {}
opcode = { ["MOVE"] = 0, ["LOADK"] = 1, ["LOADBOOL"] = 2, ["LOADNIL"] = 3,
           ["GETUPVAL"] = 4, ["GETGLOBAL"] = 5, ["GETTABLE"] = 6,
           ["SETGLOBAL"] = 7, ["SETUPVAL"] = 8, ["SETTABLE"] = 9,
           ["NEWTABLE"] = 10, ["SELF"] = 11, ["ADD"] = 12, ["SUB"] = 13,
           ["MUL"] = 14, ["DIV"] = 15, ["MOD"] = 16, ["POW"] = 17,
           ["UNM"] = 18, ["NOT"] = 19, ["LEN"] = 20, ["CONCAT"] = 21,
           ["JMP"] = 22, ["EQ"] = 23, ["LT"] = 24, ["LE"] = 25, ["TEST"] = 26,
           ["TESTSET"] = 27, ["CALL"] = 28, ["TAILCALL"] = 29, ["RETURN"] = 30,
           ["FORLOOP"] = 31, ["FORPREP"] = 32, ["TFORLOOP"] = 33, ["SETLIST"] = 34,
           ["CLOSE"] = 35, ["CLOSURE"] = 36, ["VARARG"] = 37 }

-- opcode modes
opmode = {}
opmode.iABC  = 0
opmode.iABx  = 1
opmode.iAsBx = 2

-- global options
options = {}
options.INPUT   = nil
options.OUTPUT  = "d.out"
options.GENOUT  = true

-- table to store parsed data
parsed = {}

num_par = {}
num_par["main"] = 0

-- Assembly grammar
grammar = [[
  prog <- ( <function> )* -> {}
  function <- <header> ( <instruction> )+ -> {}
  header <- ( %nl )* <s> (<user> / <main>) <s> ( %nl )+
  user <- ( "function" <s> {<name>} <s> ":" )
  main <- ( {"main"} <s> "function" <s> ":" )
  number <- <hex> / <float> / <int>
  int <- "-"? [0-9]+
  e <- [eE][+-]?[0-9]+
  float <- "-"? ([0-9]+"."[0-9]* / "."[0-9]+) <e>? / "-"? [0-9]+ <e>
  hex <- "-"? "0"[xX][0-9a-fA-F]+
  name <- [a-zA-Z_][a-zA-Z0-9_]*
  label <- <name> ":"
  string <- '"' ('\\' / '\"' / !'"' .)* '"'
  instruction <- ( <s> ( {<label>} / <number>)? <s> <ln>? <s> {<op>} ( <s> {<param>} )+ <s> ( %nl )+ ) -> {}
  op <- [A-Z]+
  param <- <register> / <number> / <name> / <string>
  register <- ( "R[" <number> "]" )
  s <- ( !%nl %s )*
  ln <- "[" <number> "]"
]]

-- prints usage message
function usage(msg)
  if msg ~= nil then
    io.write(string.format("%s: %s\n", arg[0], msg))
  end
  io.write(string.format(USAGE, arg[0], options.OUTPUT))
  os.exit(1)
end

-- parse args
function doargs()
  local i = 1
  while i <= #arg do
    if string.find(arg[i], "^-") == nil then
      options.INPUT = arg[i]
      break
    elseif arg[i] == "-h" then usage()
    elseif arg[i] == "-b" then config.ENDIANNESS = 0
    elseif arg[i] == "-o" then
      i = i + 1
      if arg[i] == nil then usage("'-o' needs argument") else options.OUTPUT = arg[i] end
    else usage(string.format("'%s' unkown option", arg[i]))
    end
    i = i + 1
  end
end

-- print table - debug only
function print_table(table)
  for k,v in pairs(table) do if k > 2 then print_table (table[k]) end end
end

-- print error message
function print_error(msg)
  io.write(string.format("ERROR: %s\n", msg))
end

-- check if is function or instructions table
function is_func_name(n)
  if n % 2 ~= 0 then return true end
  return false
end

-- parse a register
function parse_reg(r)
  local pattern = re.compile [[ reg <- "R[" {%d+} "]" -> "" ]]
  return (pattern:match(r))
end

-- parse constant type
function parse_const_type(c)
  local pattern = re.compile [[ c <- <nil> / <n> / <b> / <s>
nil <- "nil" -> "0"
b <- t / f / tt / ff
t <- "TRUE" -> "1"
f <- "FALSE" -> "1"
tt <- "true" -> "1"
ff <- "false" -> "1"
n <- float / int
int <- "-"? [0-9]+ -> "3"
float <- "-"? [0-9]+? "."? [0-9]+ ( [eE] "-"? [0-9]+)? -> "3"
s <- nc / bc
nc <- %w+ -> "4" 
bc <- '"' ( !'"' . / '\"' .)* '"' -> "4"
]]
  return (pattern:match(c))
end

-- parse boolean
function parse_bool(b)
  if b == "TRUE" then return 1 end
  return 0
end

-- get opcode mode
function get_op_mode(o)
  if o == "JMP" or o == "FORLOOP" or o == "FORPREP" then
    return opmode.iAsBx
  elseif o == "LOADK" or o == "GETGLOBAL" or o == "SETGLOBAL" or o == "CLOSURE" then
    return opmode.iABx
  end
  return opmode.iABC
end

-- set a register if it doesn't exist on table parsed
function set_reg(r, parsed)
  if not parsed["register"] then
    parsed["max_stack_size"] = 0
    parsed["register"] = {}
  end
  if not parsed["register"][r] then
    local n = parsed["max_stack_size"]
    if tonumber(r) >= parsed["max_stack_size"] then parsed["max_stack_size"] = tonumber(r)+1 end
    parsed["register"][r] = n
  end
end

-- set an opcode for instruction n
function set_op(o, parsed, n)
  parsed["instruction"][n]["O"] = o
end

-- set argument A for instruction n
function set_arg_a(a, parsed, n)
  parsed["instruction"][n]["A"] = a
end

-- set argument B for instruction n
function set_arg_b(b, parsed, n)
  parsed["instruction"][n]["B"] = b
end

-- set argument C for instruction n
function set_arg_c(c, parsed, n)
  parsed["instruction"][n]["C"] = c
end

-- set argument Bx for instruction n
function set_arg_bx(bx, parsed, n)
  parsed["instruction"][n]["Bx"] = bx
end

-- set argument sBx for instrction n
function set_arg_sbx(bx, parsed, n)
  parsed["instruction"][n]["sBx"] = bx
end

-- set a constant if it doesn't exist
function set_const(c, parsed)
  if not parsed["number_of_constants"] then
    parsed["number_of_constants"] = 0
    parsed["const"] = {}
    parsed["constant"] = {}
  end

  local t = parse_const_type(c)

  if not parsed["const"][c] then
    local n = parsed["number_of_constants"] + 1
    parsed["number_of_constants"] = n
    parsed["const"][c] = n
    parsed["constant"][n] = {}
    parsed["constant"][n]["c_type"] = t
    parsed["constant"][n]["value"] = c
  end
end

-- set a function if it doesn't exist
function set_func(f, parsed)
  if not parsed["number_of_functions"] then
    parsed["number_of_functions"] = 0
    parsed["func"] = {}
    parsed["function"] = {}
  end

  if not parsed["func"][f] then
    local n = parsed["number_of_functions"] + 1
    parsed["number_of_functions"] = n
    parsed["func"][f] = n
    parsed["function"][n] = f
  end
end

-- set an upvalue if it doesn't exist
function set_upval(u, parsed)
  if not parsed["number_of_upvalues"] then
    parsed["number_of_upvalues"] = 0
    parsed["upval"] = {}
    parsed["upvalue"] = {}
  end

  if not parsed["upval"][u] then
    local n = parsed["number_of_upvalues"] + 1
    parsed["number_of_upvalues"] = n
    parsed["upval"][u] = n
    parsed["upvalue"][n] = u
  end
end

-- check if a string is a label
function check_label(x)
  return string.find(x,":")
end

-- set a label if it doesn't exist
function set_label(label, parsed, n)
  if not parsed["label"] then parsed["label"] = {} end

  if not parsed["label"][label] then
    parsed["label"][label] = n
  else
    options.GENOUT = false
    print_error("label " .. label .. " already defined at " .. parsed["id"])
  end
end

-- parse an instruction
function parse_inst(table, parsed, n)
  local op = ""
  local a = 2
  local b = 3
  local c = 4
  if check_label(table[1]) == nil then
    op = table[1]
  else
    set_label(table[1], parsed, n)
    op = table[2]
    a = 3
    b = 4
    c = 5
  end

  set_op(op, parsed, n)

  if op ~= "JMP" then
    set_reg(parse_reg(table[a]), parsed)
    set_arg_a(parse_reg(table[a]), parsed, n)
  end

  if op == "MOVE" or op == "LOADNIL" or
     op == "UNM" or op == "NOT" or op == "LEN" then
    set_reg(parse_reg(table[b]), parsed)
    set_arg_b(parse_reg(table[b]), parsed, n)
    set_arg_c(0, parsed, n)
  elseif op == "LOADK" then
    set_const(table[b], parsed)
    set_arg_bx(parsed["const"][table[b]], parsed, n)
  elseif op == "LOADBOOL" then
    set_const(table[b], parsed)
    set_arg_b(parse_bool(table[b]), parsed, n)
    set_arg_c(table[c], parsed, n)
  elseif op == "GETUPVAL" or op == "SETUPVAL" then
    set_upval(table[b], parsed)
    set_arg_b(parsed["upval"][table[b]]-1, parsed, n)
    set_arg_c(0, parsed, n)
  elseif op == "GETGLOBAL" or op == "SETGLOBAL" then
    set_const(table[b], parsed)
    set_arg_bx(parsed["const"][table[b]], parsed, n)
  elseif op == "GETTABLE" or op == "SETTABLE" or op == "SELF" or
         op == "ADD" or op == "SUB" or op == "MUL" or
         op == "DIV" or op == "MOD" or op == "POW" or
         op == "EQ" or op == "LT" or op == "LE" then
    if not parse_reg(table[b]) then
      set_const(table[b], parsed)
      set_arg_b(max_int() + (parsed["const"][table[b]]-1), parsed, n)
    else
      set_reg(parse_reg(table[b]), parsed)
      set_arg_b(parse_reg(table[b]), parsed, n)
    end
    if not parse_reg(table[c]) then
      set_const(table[c], parsed)
      set_arg_c(max_int() + (parsed["const"][table[c]]-1), parsed, n)
    else
      set_reg(parse_reg(table[c]), parsed)
      set_arg_c(parse_reg(table[c]), parsed, n)
    end
  elseif op == "NEWTABLE" or op == "CALL" or op == "TAILCALL" or
         op == "SETLIST" then
    set_arg_b(table[b], parsed, n)
    set_arg_c(table[c], parsed, n)
  elseif op == "CONCAT" then
    set_reg(parse_reg(table[b]), parsed)
    set_arg_b(parse_reg(table[b]), parsed, n)
    set_reg(parse_reg(table[c]), parsed)
    set_arg_c(parse_reg(table[c]), parsed, n)
  elseif op == "JMP" then
    set_arg_a(0, parsed, n)
    set_arg_sbx(table[a], parsed, n)
  elseif op == "FORLOOP" or op == "FORPREP" then
    set_arg_sbx(table[b], parsed, n)
  elseif op == "TEST" or op == "TESTSET" then
    set_reg(parse_reg(table[b]), parsed)
    set_arg_b(parse_reg(table[b]), parsed, n)
    set_arg_c(parse_bool(table[c]), parsed, n)
  elseif op == "RETURN" then
    set_arg_b(table[b], parsed, n)
    set_arg_c(0, parsed, n)
  elseif op == "TFORLOOP" then
    set_arg_b(0, parsed, n)
    set_arg_c(table[b], parsed, n)
  elseif op == "CLOSE" then
    set_arg_b(0, parsed, n)
    set_arg_c(0, parsed, n)
  elseif op == "CLOSURE" then
    set_func(table[b], parsed)
    set_arg_bx(parsed["func"][table[b]], parsed, n)
    num_par[table[b]] = tonumber(table[c])
  elseif op == "VARARG" then
    set_arg_b(table[b], parsed, n)
    set_arg_c(0, parsed, n)
    parsed["is_vararg"] = table[b] - 1
    num_par[parsed["id"]] = 0
  else
    -- shouldn't enter here
    options.GENOUT = false
    print_error(op .. " : not a valid operator")
  end
end

-- parse a function
function parse_func(table, parsed)
  parsed["instruction"] = {}
  local n = 0
  for k in pairs (table) do
    parsed["instruction"][k] = {}
    parse_inst(table[k], parsed, k)
    n = k
  end
  parsed["number_of_instructions"] = n
end

-- set a vararg function
function set_vararg(parsed, f)
  if f == "main" then parsed["is_vararg"] = 2 else parsed["is_vararg"] = 0 end
end

-- fix function values
function fix_function_values(parsed)
  if parsed["max_stack_size"] < 2 then parsed["max_stack_size"] = 2 end
  if not parsed["number_of_parameters"] then parsed["number_of_parameters"] = 0 end
  if not parsed["number_of_upvalues"] then parsed["number_of_upvalues"] = 0 end
end

-- start parsing process
function parse_all(table, parsed)
  local f
  for k,v in pairs (table) do
    if is_func_name(k) then
      f = v
      parsed[f] = {}
      parsed[f]["id"] = f
    else
      set_vararg(parsed[f], f)
      parse_func(table[k], parsed[f])
      fix_function_values(parsed[f])
    end
  end
end

-- write a byte to output
function write_byte(output, byte)
  if byte ~= 0 then
    output:write(string.format("%c", byte))
  else
    output:write('\0')
  end
end

-- write source name to output
function write_source_name(output, name)
  write_str(output, name)
end

-- write header to output
function write_header(output)
  for i=1,string.len(config.SIGNATURE) do
    write_byte (output, string.byte(config.SIGNATURE,i))
  end
  write_byte (output, config.VERSION)
  write_byte (output, config.FORMAT)
  write_byte (output, config.ENDIANNESS)
  write_byte (output, config.INT)
  write_byte (output, config.SIZE_T)
  write_byte (output, config.INSTRUCTION)
  write_byte (output, config.LUA_NUMBER)
  write_byte (output, config.INTEGRAL)
end

-- gen max int
function max_int()
  return ( 2 ^ 8 )
end

-- converts int to little endian
function gen_int(n)
  local t = {}
  n = math.floor(n)
  if n >= 0 then
    for i=1,config.INT do
      t[i] = n % max_int()
      n = math.floor(n / max_int())
    end
  end
  return t
end

-- writes an integer
function write_int(output, n)
  local i = gen_int(n)
  if config.ENDIANNESS == 1 then
    for k=1,config.INT do write_byte(output, i[k]) end
  else
    for k=config.INT,1,-1 do write_byte(output, i[k]) end
  end
end

-- write function values
function write_function_values(output, parsed)
  parsed["number_of_parameters"] = num_par[parsed["id"]]
  write_byte(output,parsed["number_of_upvalues"])
  write_byte(output,parsed["number_of_parameters"])
  write_byte(output,parsed["is_vararg"])
  write_byte(output,parsed["max_stack_size"])
end

-- gen iABC instruction
function gen_iABC(O, A, B, C)
  local field = {O, A, C, B}
  local v, i = {}, 0
  local cValue, cBits, cPos = 0, 0, 1
  -- encode an instruction
  while i < config.INSTRUCTION do
    -- if need more bits, suck in a field at a time
    while cBits < 8 do
      cValue = field[cPos] * math.ldexp(1, cBits) + cValue
      cBits = cBits + config.iABC[cPos]; cPos = cPos + 1
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

-- gen iABx instruction
function gen_iABx(O, A, Bx)
  return gen_iABC(O, A, math.floor(Bx / config.MASK_C), (Bx % config.MASK_C))
end

-- gen iAsBx instruction
function gen_iAsBx(O, A, sBx)
  return gen_iABx(O, A, (sBx + config.MAXARG_sBx))
end

-- write instruction to output
function write_instruction(output, parsed, n)
  local i = parsed["instruction"][n]
  local o = opcode[i["O"]]
  local m = get_op_mode(i["O"])
  local t = {}

  if m == opmode.iABC then
    t = gen_iABC(o,i["A"],i["B"],i["C"])
  elseif m == opmode.iABx then
    t = gen_iABx(o,i["A"],i["Bx"] - 1)
  elseif m == opmode.iAsBx then
    local sbx
    if tonumber(parse_const_type(i["sBx"])) == types.LUA_TSTRING then
      local label = i["sBx"] .. ":"
      if not parsed["label"][label] then
        sbx = 1
        print_error("label " .. label .. " not defined at " .. parsed["id"])
      else
        sbx = (parsed["label"][label] - n) - 1
      end
    else
      sbx = (i["sBx"] - n) - 1
    end
    t = gen_iAsBx(o,i["A"], sbx)
  end

  if config.ENDIANNESS == 1 then
    for k=1,config.INSTRUCTION do write_byte(output, t[k]) end
  else
    for k=config.INSTRUCTION,1,-1 do write_byte(output, t[k]) end
  end
end

-- write all instructions
function write_instructions(output, parsed)
  -- number of instructions
  write_int(output, parsed["number_of_instructions"])
  for i=1,parsed["number_of_instructions"] do
    write_instruction(output, parsed, i)
  end
end

-- gen a string length
function gen_len(n)
  local t = {}
  n = math.floor(n)
  if n >= 0 then
    for i=1,config.SIZE_T do
      t[i] = n % max_int()
      n = math.floor(n / max_int())
    end
  end
  return t
end

-- write string length to output
function write_len(output, n)
  local l = gen_len(n)
  if config.ENDIANNESS == 1 then
    for k=1,config.SIZE_T do write_byte(output, l[k]) end
  else
    for k=config.SIZE_T,1,-1 do write_byte(output, l[k]) end
  end
end

-- fix a string
function fix_str(str)
  if string.find(str, "\\\"") then str = string.gsub(str, "\\\"", '"') end
  if string.find(str, "\\\\") then str = string.gsub(str, "\\\\", '\\') end
  if string.find(str, "\\a") then str = string.gsub(str, "\\a", '\a') end
  if string.find(str, "\\b") then str = string.gsub(str, "\\b", '\b') end
  if string.find(str, "\\f") then str = string.gsub(str, "\\f", '\f') end
  if string.find(str, "\\n") then str = string.gsub(str, "\\n", '\n') end
  if string.find(str, "\\r") then str = string.gsub(str, "\\r", '\r') end
  if string.find(str, "\\t") then str = string.gsub(str, "\\t", '\t') end
  if string.find(str, "\\v") then str = string.gsub(str, "\\v", '\v') end
  if string.find(str, "\\0") then str = string.gsub(str, "\\0", "\0") end
  if string.find(str, "\\[0-9]+") then str = string.gsub(str, "\\([0-9]+)", function (s) return string.format("%c", tonumber(s)) end) end
  return str
end

-- write a string to output
function write_str(output, str)
  if (string.byte(str, 1) == 34 or string.byte(str, 1) == 39) and
     (string.byte(str, string.len(str)) == 34 or string.byte(str, string.len(str)) == 39) then
    str = string.sub(str, 2, string.len(str) - 1)
    str = fix_str(str)
  end
  local len = string.len(str)
  write_len(output, len + 1)
  if config.ENDIANNESS == 1 then
    for i=1,len do write_byte(output, string.byte(str, i)) end
    write_byte(output, 0)
  else
    wirte_byte(output, 0)
    for i=len,1,-1 do write_byte(output, string.byte(str, i)) end
  end
end

-- get a byte
function get_byte(v)
  return math.floor(v / 256), string.char(math.floor(v) % 256)
end

-- converts float to little endian
function convert_number(x)
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

-- write a float to output
function write_number(output, n)
  local t = convert_number(tonumber(n))
  if config.ENDIANNESS == 1 then
    for k=1,config.LUA_NUMBER do write_byte(output, t[k]) end
  else
    for k=config.LUA_NUMBER,1,-1 do write_byte(output, t[k]) end
  end
end

-- write a constant to output
function write_constant(output, parsed)
  local t = tonumber(parsed["c_type"])
  write_byte(output, t)
  if t == types.LUA_TNIL then
    -- do not need to right anything
  elseif t == types.LUA_TBOOLEAN then
    if parsed["value"] == "TRUE" or parsed["value"] == "true" then write_byte(output, 1) else write_byte(output, 0) end
  elseif t == types.LUA_TNUMBER then
    write_number(output, parsed["value"])
  elseif t == types.LUA_TSTRING then
    write_str(output, parsed["value"])
  end
end

-- write all constants
function write_constants(output, parsed)
  if not parsed["number_of_constants"] then parsed["number_of_constants"] = 0 end
  write_int(output, parsed["number_of_constants"])
  for i=1,parsed["number_of_constants"] do
    write_constant(output, parsed["constant"][i])
  end
end

-- write locals to output
function write_locals(output, parsed)
  if not parsed["number_of_locals"] then parsed["number_of_locals"] = 0 end
  write_int(output, parsed["number_of_locals"])
  for i=1,parsed["number_of_locals"] do
    write_str(output, parsed["local"][i])
  end
end

-- write upvalues to output
function write_upvalues(output, parsed)
  if not parsed["number_of_upvalues"] then parsed["number_of_upvalues"] = 0 end
  write_int(output, parsed["number_of_upvalues"])
  for i=1,parsed["number_of_upvalues"] do
    write_str(output, parsed["upvalue"][i])
  end
end

-- write a function block
function write_function_block(output, parsed, to_be_parsed)
  local nf = 0
  -- line defined
  write_int(output, 0)
  -- last line defined
  write_int(output, 0)
  write_function_values(output, parsed)
  write_instructions(output, parsed)
  write_constants(output, parsed)
  if parsed["number_of_functions"] then nf = parsed["number_of_functions"] end
  write_int(output, nf)
  for n=1,nf do
    write_len(output, 0)
    write_function_block(output, to_be_parsed[parsed["function"][n]], to_be_parsed)
  end
  -- source line positions
  write_int(output, 0)
  -- locals
  write_int(output, 0)
  -- upvalues
  write_int(output, 0)
end

-- write binary file
function write_bin(output, parsed)
  write_header(output)
  write_source_name(output, "@")
  write_function_block(output, parsed["main"], parsed)
end

-- do a pre scan at file to be parsed
function scan_file(file)
  local scan = {}
  local f
  local main = true
  for line in io.lines(file) do
    if not string.byte(line) then -- new line, do nothing
    elseif string.find(line, "main function:") and main then
      f = "main"
      scan[f] = {}
      scan[f]["line"] = {}
      scan[f]["noi"] = 0
      main = false
    elseif string.find(line, "function [a-zA-Z_][a-zA-Z0-9_]*:") then
      f = string.gsub(string.gsub(string.gsub(line,"function", ""),":",""),"%s+","")
      scan[f] = {}
      scan[f]["line"] = {}
      scan[f]["noi"] = 0
    else
      if f then scan[f]["noi"] = scan[f]["noi"] + 1 ; scan[f]["line"][scan[f]["noi"]] = line end
    end
  end
  return scan
end

-- check if file could be parsed
function check_table(table, scan)
  local f
  if not table[1] then
    print_error("main could not be parsed")
    return false
  end
  for k,v in pairs (table) do
    if is_func_name(k) then
      f = v
    else
      if scan[f]["noi"] ~= #v then
        print_error("at function " .. f .. " could not parse instruction " .. #v + 1)
        print (scan[f]["line"][#v + 1])
        return false
      end
    end
  end
  return true
end

-- check if file exists
function exists(file)
  local f = io.open(file, "r")
  if f then
    f:close()
    return true
  end
  return false
end

if #arg < 1 then
  usage("no input file given");
end

doargs()

if options.INPUT == nil then
  usage("no input file given")
end

if not exists(options.INPUT) then
  usage(options.INPUT .. " not found")
end

local parser = re.compile(grammar)

input = io.open(options.INPUT, "r")
contents = input:read("*a")
input:close()

local scan = scan_file(options.INPUT)

local table = parser:match(contents)

if check_table(table, scan) then parse_all(table, parsed) else options.GENOUT = false end

if options.GENOUT then
  output = io.open(options.OUTPUT, "wb")
  write_bin(output, parsed)
  output:close()
end

os.exit(0)
