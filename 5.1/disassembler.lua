--[[
Lua Disassembler
Version 0.1
Author: Andre Murbach Maidl
]]

-- usage message
USAGE = [[
usage: %s [options] [filename]
Available options are:
-h	print this help
-l	list on luac style
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
config.BLOCK       = 4
config.HEADERBLOCK = 12
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

-- global types
types = {}
types.LUA_TNIL     = 0
types.LUA_TBOOLEAN = 1
types.LUA_TNUMBER  = 3
types.LUA_TSTRING  = 4

-- opcodes
opcode = {}
opcode = { "MOVE", "LOADK", "LOADBOOL", "LOADNIL",
           "GETUPVAL", "GETGLOBAL", "GETTABLE",
           "SETGLOBAL", "SETUPVAL", "SETTABLE",
           "NEWTABLE", "SELF", "ADD", "SUB", "MUL", "DIV",
           "MOD", "POW", "UNM", "NOT", "LEN", "CONCAT",
           "JMP", "EQ", "LT", "LE", "TEST", "TESTSET",
           "CALL", "TAILCALL", "RETURN", "FORLOOP",
           "FORPREP", "TFORLOOP", "SETLIST", "CLOSE",
           "CLOSURE", "VARARG" }

-- opcode modes
opmode = {}
opmode.iABC  = 0
opmode.iABx  = 1
opmode.iAsBx = 2

-- opcode masks
opargmask = {}
opargmask.OpArgN = 0
opargmask.OpArgU = 1
opargmask.OpArgR = 2
opargmask.OpArgK = 3

-- global options
options = {}
options.INPUT   = nil
options.OUTPUT  = "d.asm"
options.LISTING = false
options.TMP     = "luac_tmp.out"

-- table to store parsed data
parsed = {}

-- prints usage message
function usage(msg)
  if msg ~= nil then
    io.write(string.format("%s: %s\n", arg[0], msg))
  end
  io.write(string.format(USAGE, arg[0], options.OUTPUT))
  os.exit(1)
end

-- parse arguments
function doargs()
  local i = 1
  while i <= #arg do
    if string.find(arg[i], "^-") == nil then
      options.INPUT = arg[i]
      break
    elseif arg[i] == "-h" then usage()
    elseif arg[i] == "-l" then
      options.LISTING = true
    elseif arg[i] == "-o" then
      i = i + 1
      if arg[i] == nil then usage("'-o' needs argument") else options.OUTPUT = arg[i] end
    else usage(string.format("'%s' unkown option", arg[i]))
    end
    i = i + 1
  end
end

-- print a table - debug only
function print_table(table)
  for k,p in pairs(table) do print (k,p) end
end

-- print a chunk - debug only
function print_chunk(binary_chunk)
  for b in string.gfind(binary_chunk, ".") do
    io.write(string.format("%d ", string.byte(b)))
  end
  io.write(string.rep("    ", config.BLOCK - string.len(binary_chunk) + 1))
  io.write(string.gsub(binary_chunk, "%c", "."), "\n")
end

-- print what still needs to be parsed - debug only
function print_rest(file)
  while true do
    binary_chunk = file:read(config.BLOCK)
    if not binary_chunk then break else print_chunk(binary_chunk) end
  end
end

-- get a bit from an hex string
function get_bit(x,a,b)
  return (math.floor((x / 2^a) % 2^b))
end

-- generate an hex string
function get_hex_string(x)
  str = "0x"
  if config.ENDIANNESS == 1 then
    for i=config.BLOCK,1,-1 do
      str = str .. string.format("%02x", string.byte(x,i))
    end
  else
    for i=1,config.BLOCK do
      str = str .. string.format("%02x", string.byte(x,i))
    end
  end

  return str
end

-- get an int from a chunk
function get_int(binary_chunk)
  i = 0
  if config.ENDIANNESS == 1 then
    for p=config.INT,2,-1 do
      i = i + (256 * string.byte(binary_chunk,p))
    end
    i = i + string.byte(binary_chunk,1)
  else
    for p=1,config.INT-1 do
      i = i + (256 * string.byte(binary_chunk,p))
    end
    i = i + string.byte(binary_chunk,config.INT)
  end
  return i
end

-- revert chunk
function revert(x)
  reverted = ""
  for k=config.LUA_NUMBER,1,-1 do
    reverted = reverted .. string.format("%c", string.byte(x,k))
  end
  return reverted
end

-- convert little endian number to float
function convert_little_endian(x)
  sign = 1
  mantissa = string.byte(x, 7) % 16
  for i=6,1,-1 do mantissa = mantissa * 256 + string.byte(x,i) end
  if string.byte(x,8) > 127 then sign = -1 end
  exponent = (string.byte(x,8) % 128) * 16 + math.floor(string.byte(x,7) / 16)
  if exponent == 0 then return 0 end
  mantissa = (math.ldexp(mantissa, -52) + 1) * sign
  return math.ldexp(mantissa, exponent - 1023)
end

-- convert big endian number to float
function convert_big_endian(x)
  sign = 1
  mantissa = string.byte(x, 2) % 16
  for i=3,config.LUA_NUMBER do mantissa = mantissa * 256 + string.byte(x,i) end
  if string.byte(x,1) > 127 then sign = -1 end
  exponent = (string.byte(x,1) % 128) * 16 + math.floor(string.byte(x,2) / 16)
  if exponent == 0 then return 0 end
  mantissa = (math.ldexp(mantissa, -52) + 1) * sign
  return math.ldexp(mantissa, exponent - 1023)
end

-- convert a number
function convert_number(x)
  if config.ENDIANNESS == 1 then
    return convert_little_endian(x)
  end

  return convert_big_endian(x)
end

-- get string length
function get_str_len(binary_chunk)
  if config.ENDIANNESS == 1 then
    i = string.byte(binary_chunk,1)
    for p=2,config.LUA_NUMBER do
      i = i + (256 * string.byte(binary_chunk,p))
    end
  else
    i = string.byte(binary_chunk,config.INT)
    for p=config.INT-1,1,-1 do
      i = i + (256 * string.byte(binary_chunk,p))
    end
  end
  return i
end

-- get boolean value
function get_boolean(binary_chunk)
  if string.byte(binary_chunk,1) == 0 then return "false" end
  return "true"
end

-- check if is a control char
function iscntrl(x)
  if (x >= 0 and x <= 31) or (x == 127) then return true end
  return false
end

-- check if is a printable char
function isprint(x)
  return not iscntrl(x)
end

-- load header from file
function load_header(file)
  return file:read(config.HEADERBLOCK)
end

-- check Lua signature
function check_signature(header_chunk)
  if (string.sub(header_chunk, 1, string.len(config.SIGNATURE)) == config.SIGNATURE) then
    return true
  end

  return false
end

-- check Lua version for 5.1
function check_version(header_chunk)
  if (string.byte(header_chunk,5) == config.VERSION) then
    return true
  end

  return false
end

-- set flags detected from file
function set_auto_flags(header_chunk)
  config.ENDIANNESS  = string.byte(header_chunk,7)
  config.INT         = string.byte(header_chunk,8)
  config.SIZE_T      = string.byte(header_chunk,9)
  config.INSTRUCTION = string.byte(header_chunk,10)
  config.LUA_NUMBER  = string.byte(header_chunk,11)
  config.INGETRAL    = string.byte(header_chunk,12)
end

-- get a string based on its length
function get_string(file, str_len)
  binary_chunk = file:read(str_len)
  local str = ""
  for i=1,str_len do
    local byte = string.byte(binary_chunk, i)
    if byte ~= 0 then
      str = str .. string.format("%c", string.byte(binary_chunk,i))
    else
      str = str .. '\0'
    end
  end

  return str
end

-- get source name from file
function get_source_name(file)
  binary_chunk = file:read(config.SIZE_T)
  str_len = get_str_len(binary_chunk)
  return get_string(file, str_len)
end

-- get line defined from file
function get_line_defined(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["line_defined"] = get_int(binary_chunk)
end

-- get last line defined from file
function get_last_line_defined(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["last_line_defined"] = get_int(binary_chunk)
end

-- get function values from file
function get_function_values(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["number_of_upvalues"]   = string.byte(binary_chunk,1)
  parsed["number_of_parameters"] = string.byte(binary_chunk,2)
  parsed["is_vararg"]            = string.byte(binary_chunk,3)
  parsed["max_stack_size"]       = string.byte(binary_chunk,4)
end

-- get instruction from file
function get_instruction(file, parsed, n)
  if not parsed["instruction"] then parsed["instruction"] = {} end

  binary_chunk = file:read(config.BLOCK)
  parsed["instruction"][n] = get_hex_string(binary_chunk)
end

-- get all instructions from file
function get_instructions(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["number_of_instructions"] = get_int(binary_chunk)

  for n=1,parsed["number_of_instructions"] do
    get_instruction(file, parsed, n)
  end
end

-- get a constant from file
function get_constant(file, parsed, n)
  if not parsed["constant"] then parsed["constant"] = {} end
  if not parsed["constant"][n] then parsed["constant"][n] = {} end

  binary_chunk = file:read(1)
  c_type = string.byte(binary_chunk,1)

  if c_type == types.LUA_TNIL then
    parsed["constant"][n]["c_type"] = types.LUA_TNIL
    parsed["constant"][n]["value"] = "nil"
  elseif c_type == types.LUA_TBOOLEAN then
    parsed["constant"][n]["c_type"] = types.LUA_TBOOLEAN
    binary_chunk = file:read(1)
    parsed["constant"][n]["value"] = get_boolean(binary_chunk)
  elseif c_type == types.LUA_TNUMBER then
    parsed["constant"][n]["c_type"] = types.LUA_TNUMBER
    binary_chunk = file:read(config.LUA_NUMBER)
    parsed["constant"][n]["value"] = convert_number(binary_chunk)
  elseif c_type == types.LUA_TSTRING then
    parsed["constant"][n]["c_type"] = types.LUA_TSTRING
    binary_chunk = file:read(config.SIZE_T)
    parsed["constant"][n]["str_len"] = get_str_len(binary_chunk)
    parsed["constant"][n]["value"] = get_string(file, parsed["constant"][n]["str_len"])
  end
end

-- get all constants from file
function get_constants(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["number_of_constants"] = get_int(binary_chunk)

  for n=1,parsed["number_of_constants"] do
    get_constant(file, parsed, n)
  end
end

-- get number of functions from file
function get_number_of_functions(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["number_of_functions"] = get_int(binary_chunk)
end

-- get source line positions from file
function get_source_line_position(file, parsed, n)
  if not parsed["instruction_line"] then parsed["instruction_line"] = {} end

  binary_chunk = file:read(config.BLOCK)
  parsed["instruction_line"][n] = get_int(binary_chunk)
end

-- get all source line positions from file
function get_source_line_positions(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["number_of_instruction_lines"] = get_int(binary_chunk)

  for n=1,parsed["number_of_instruction_lines"] do
    get_source_line_position(file, parsed, n)
  end
end

-- get a local variable from file
function get_local(file, parsed, n)
  if not parsed["local"] then parsed["local"] = {} end
  if not parsed["local"][n] then parsed["local"][n] = {} end

  binary_chunk = file:read(config.SIZE_T)
  parsed["local"][n]["str_len"] = get_str_len(binary_chunk)
  parsed["local"][n]["varname"] = get_string(file, parsed["local"][n]["str_len"])
  binary_chunk = file:read(config.BLOCK)
  parsed["local"][n]["startpc"] = string.byte(binary_chunk,1)
  binary_chunk = file:read(config.BLOCK)
  parsed["local"][n]["endpc"] = string.byte(binary_chunk,1)
end

-- get all local variables from file
function get_locals(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["number_of_locals"] = get_int(binary_chunk)

  for n=1,parsed["number_of_locals"] do
    get_local(file, parsed, n)
  end
end

-- get an upvalue from file
function get_upvalue(file, parsed, n)
  if not parsed["upvalue"] then parsed["upvalue"] = {} end
  if not parsed["upvalue"][n] then parsed["upvalue"][n] = {} end

  binary_chunk = file:read(config.SIZE_T)
  parsed["upvalue"][n]["str_len"] = get_str_len(binary_chunk)
  parsed["upvalue"][n]["value"] = get_string(file, parsed["upvalue"][n]["str_len"])
end

-- get all upvalues from file
function get_upvalues(file, parsed)
  binary_chunk = file:read(config.BLOCK)
  parsed["size_of_upvalues"] = get_int(binary_chunk)

  for n=1,parsed["size_of_upvalues"] do
    get_upvalue(file, parsed, n)
  end
end

-- get function block from file
function get_function_block(file, parsed, source_name, parent)
  parsed["source_name"] = source_name
  get_line_defined(file, parsed)
  get_last_line_defined(file, parsed)
  get_function_values(file, parsed)
  get_instructions(file, parsed)
  get_constants(file, parsed)
  get_number_of_functions(file, parsed)
  for n=1,parsed["number_of_functions"] do
    binary_chunk = file:read(config.SIZE_T)
    if not parsed["function"] then parsed["function"] = {} end
    if not parsed["function"][n] then parsed["function"][n] = {} end
    parsed["function"][n]["parent"] = parent
    parsed["function"][n]["id"] = n
    get_function_block(file, parsed["function"][n], source_name, parent+1)
  end
  get_source_line_positions(file, parsed)
  get_locals(file, parsed)
  get_upvalues(file, parsed)
end

-- output header
function print_header(parsed)

  local function SS(x) if x == 1 then return "" else return "s" end end
  local function S(x) return x,SS(X) end

  func_name = "function"
  is_vararg = ""

  if parsed["line_defined"] == 0 and parsed["id"] == 0 then func_name = "main" end
  if parsed["is_vararg"] ~= 0 then is_vararg = "+" end

  io.write(string.format("\n%s <%s:%d,%d> (%d instruction%s, %d bytes at %d)\n",
			  func_name, parsed["source_name"], parsed["line_defined"], parsed["last_line_defined"],
			  parsed["number_of_instructions"], SS(parsed["number_of_instructions"]),
			  parsed["number_of_instructions"] * config.BLOCK, 0))
  io.write(string.format("%d%s param%s, %d slot%s, %d upvalue%s, ",
			  parsed["number_of_parameters"], is_vararg, SS(parsed["number_of_parameters"]),
			  parsed["max_stack_size"], SS(parsed["max_stack_size"]),
			  parsed["number_of_upvalues"], SS(parsed["number_of_upvalues"])))
 io.write(string.format("%d local%s, %d constant%s, %d function%s\n",
			  parsed["number_of_locals"], SS(parsed["number_of_locals"]),
			  parsed["number_of_constants"], SS(parsed["number_of_constants"]),
			  parsed["number_of_functions"], SS(parsed["number_of_functions"])))

end

-- get an opcode
function get_opcode(inst)
  return get_bit(inst,config.POS_OP,config.SIZE_OP)
end

-- get argument A
function getarg_a(inst)
  return get_bit(inst,config.POS_A,config.SIZE_A)
end

-- get argument B
function getarg_b(inst)
  return get_bit(inst,config.POS_B,config.SIZE_B)
end

-- get argument C
function getarg_c(inst)
  return get_bit(inst,config.POS_C,config.SIZE_C)
end

-- get argument Bx
function getarg_bx(inst)
  return get_bit(inst,config.POS_Bx,config.SIZE_Bx)
end

-- get max argument for Bx
function maxarg_bx()
  return (2 ^ config.SIZE_Bx) - 1
end

-- get max argument for sBx
function maxarg_sbx()
  return get_bit(maxarg_bx(),1,32)
end

-- get argument sBx
function getarg_sbx(inst)
  return getarg_bx(inst)-maxarg_sbx()
end

-- get opcode mode
function get_op_mode(o)
  if opcode[o] == "JMP" or opcode[o] == "FORLOOP" or opcode[o] == "FORPREP" then
    return opmode.iAsBx
  elseif opcode[o] == "LOADK" or opcode[o] == "GETGLOBAL" or
         opcode[o] == "SETGLOBAL" or opcode[o] == "CLOSURE" then
    return opmode.iABx
  end
  return opmode.iABC
end

-- get B mask
function get_b_mode(o)
  if opcode[o] == "TFORLOOP" or opcode[o] == "CLOSE" then
    m = opargmask.OpArgN
  elseif opcode[o] == "LOADBOOL" or opcode[o] == "GETUPVAL" or
         opcode[o] == "SETUPVAL" or opcode[o] == "NEWTABLE" or
         opcode[o] == "CALL" or opcode[o] == "TAILCALL" or
         opcode[o] == "RETURN" or opcode[o] == "SETLIST" or
         opcode[o] == "CLOSURE" or opcode[o] == "VARARG" then
    m = opargmask.OpArgU
  elseif opcode[o] == "MOVE" or opcode[o] == "LOADNIL" or
         opcode[o] == "GETTABLE" or opcode[o] == "SELF" or
         opcode[o] == "UNM" or opcode[o] == "NOT" or
         opcode[o] == "LEN" or opcode[o] == "CONCAT" or
         opcode[o] == "JMP" or opcode[o] == "TEST" or
         opcode[o] == "TESTSET" or opcode[o] == "FORLOOP" or opcode[o] == "FORPREP" then
    m = opargmask.OpArgR
   elseif opcode[o] == "LOADK" or opcode[o] == "GETGLOBAL" or
          opcode[o] == "SETGLOBAL" or opcode[o] == "SETTABLE" or
          opcode[o] == "ADD" or opcode[o] == "SUB" or opcode[o] == "MUL" or
          opcode[o] == "DIV" or opcode[o] == "MOD" or opcode[o] == "POW" or
          opcode[o] == "EQ" or opcode[o] == "LT" or opcode[o] == "LE" then
    m = opargmask.OpArgK
  end
  return m
end

-- get C mask
function get_c_mode(o)
  if opcode[o] == "MOVE" or opcode[o] == "LOADK" or
     opcode[o] == "LOADNIL" or opcode[o] == "GETUPVAL" or
     opcode[o] == "GETGLOBAL" or opcode[o] == "SETGLOBAL" or
     opcode[o] == "SETUPVAL" or opcode[o] == "UNM" or
     opcode[o] == "NOT" or opcode[o] == "LEN" or opcode[o] == "JMP" or
     opcode[o] == "RETURN" or opcode[o] == "FORLOOP" or
     opcode[o] == "FORPREP" or opcode[o] == "CLOSE" or
     opcode[o] == "CLOSURE" or opcode[o] == "VARARG" then
    m = opargmask.OpArgN
  elseif opcode[o] == "LOADBOOL" or opcode[o] == "NEWTABLE" or
         opcode[o] == "TEST" or opcode[o] == "TESTSET" or
         opcode[o] == "CALL" or opcode[o] == "TAILCALL" or
         opcode[o] == "TFORLOOP" or opcode[o] == "SETLIST" then
    m = opargmask.OpArgU
  elseif opcode[o] == "CONCAT" then
    m = opargmask.OpArgR
  elseif opcode[o] == "GETTABLE" or opcode[o] == "SETTABLE" or
         opcode[o] == "SELF" or opcode[o] == "ADD" or opcode[o] == "SUB" or
         opcode[o] == "MUL" or opcode[o] == "DIV" or opcode[o] == "MOD" or
         opcode[o] == "POW" or opcode[o] == "EQ" or opcode[o] == "LT" or opcode[o] == "LE" then
    m = opargmask.OpArgK
  end
  return m
end

-- get BITRK
function bitrk()
  return (2 ^ (config.SIZE_B - 1))
end

-- get ~BITRK
function not_bitrk()
  return (bitrk() + 1) * -1
end

-- get ISK
function isk(x)
  if x >= bitrk() then return true end
  return false
end

-- get INDEXK
function indexk(x)
  if x >= bitrk() then return not_bitrk() + (x + 1) else return x end
end

-- output string
function print_string(parsed)
  io.write(string.format("\""))
  for i=1,parsed["str_len"]-1 do
    char = string.byte(parsed["value"],i)
    if char == 34 then io.write(string.format("\\\""))
    elseif char == 92 then io.write(string.format("\\\\"))
    elseif char == 7 then io.write(string.format("\\a"))
    elseif char == 8 then io.write(string.format("\\b"))
    elseif char == 12 then io.write(string.format("\\f"))
    elseif char == 10 then io.write(string.format("\\n"))
    elseif char == 13 then io.write(string.format("\\r"))
    elseif char == 9 then io.write(string.format("\\t"))
    elseif char == 11 then io.write(string.format("\\v"))
    else
      if isprint(char) then
        io.write(string.format("%c", char))
      else
        io.write(string.format("\\%03d", char))
      end
    end
  end
  io.write(string.format("\""))
end

-- output a constant
function print_constant(parsed, n)
  c_type = parsed["constant"][n]["c_type"]
  value = parsed["constant"][n]["value"]
  if c_type == types.LUA_TNIL then
    io.write(string.format("%s", value))
  elseif c_type == types.LUA_TBOOLEAN then
    io.write(string.format("%s", value))
  elseif c_type == types.LUA_TNUMBER then
    io.write(string.format("%s", value))
  elseif c_type == types.LUA_TSTRING then
    print_string(parsed["constant"][n])
  end
end

-- output code
function print_code(parsed)
  for n=1,parsed["number_of_instructions"] do
    i = parsed["instruction"][n]
    o = get_opcode(i) + 1
    a = getarg_a(i)
    b = getarg_b(i)
    c = getarg_c(i)
    bx = getarg_bx(i)
    sbx = getarg_sbx(i)
    if parsed["number_of_instruction_lines"] > 0 then line = parsed["instruction_line"][n] else line = 0 end

    io.write(string.format("\t%d\t", n))
    if line > 0 then
      io.write(string.format("[%d]\t", line))
    else
      io.write(string.format("[-]\t"))
    end
    io.write(string.format("%-9s\t", opcode[o]))

    if get_op_mode(o) == opmode.iABC then
      io.write(string.format("%d", a))
      if get_b_mode(o) ~= opargmask.OpArgN then
        if isk(b) then
          io.write(string.format(" %d", -1-indexk(b)))
        else
          io.write(string.format(" %d", b))
        end
      end
      if get_c_mode(o) ~= opargmask.OpArgN then
        if isk(c) then
          io.write(string.format(" %d", -1-indexk(c)))
        else
          io.write(string.format(" %d", c))
        end
      end
    elseif get_op_mode(o) == opmode.iABx then
      if get_b_mode(o) == opargmask.OpArgK then
        io.write(string.format("%d %d", a, -1-bx))
      else
        io.write(string.format("%d %d", a, bx))
      end
    elseif get_op_mode(o) == opmode.iAsBx then
      if opcode[o] == "JMP" then
        io.write(string.format("%d", sbx))
      else
        io.write(string.format("%d %d", a, sbx))
      end
    end

    if opcode[o] == "LOADK" then
      io.write(string.format("\t; "))
      print_constant(parsed, bx+1)
    elseif opcode[o] == "GETUPVAL" or opcode[o] == "SETUPVAL" then
      if parsed["number_of_upvalues"] > 0 then myupvalue = parsed["upvalue"][b+1]["value"] else myupvalue = "-" end
      io.write(string.format("\t; %s", myupvalue))
    elseif opcode[o] == "GETGLOBAL" or opcode[o] == "SETGLOBAL" then
      io.write(string.format("\t; %s", parsed["constant"][bx+1]["value"]))
    elseif opcode[o] == "GETTABLE" or opcode[o] == "SELF" then
      if isk(c) then
        io.write(string.format("\t; "))
        print_constant(parsed, indexk(c+1))
      end
    elseif opcode[o] == "SETTABLE" or opcode[o] == "ADD" or
           opcode[o] == "SUB" or opcode[o] == "MUL" or
           opcode[o] == "DIV" or opcode[o] == "POW" or
           opcode[o] == "EQ" or opcode[o] == "LT" or opcode[o] == "LE" then
      if isk(b) or isk(c) then
        io.write(string.format("\t; "))
        if isk(b) then print_constant(parsed, indexk(b+1)) else io.write(string.format("-")) end
        io.write(string.format(" "))
        if isk(c) then print_constant(parsed, indexk(c+1)) else io.write(string.format("-")) end
      end
    elseif opcode[o] == "JMP" or opcode[o] == "FORLOOP" or opcode[o] == "FORPREP" then
      io.write(string.format("\t; to %d", sbx+n+1))
    elseif opcode[o] == "CLOSURE" then
      io.write(string.format("\t; 0x"))
    elseif opcode[o] == "SETLIST" then
      if c == 0 then
        io.write(string.format("\t; %d", 0))
      else
        io.write(string.format("\t; %d", c))
      end
    end

    io.write(string.format("\n"))
  end
end

-- output a function
function print_function(parsed)
  print_header(parsed)
  print_code(parsed)

  for i=1,parsed["number_of_functions"] do
    print_function(parsed["function"][i])
  end
end

-- output header to a file
function write_header(output, parsed)
  if parsed["line_defined"] == 0 and parsed["id"] == 0 then
    func_name = "main function:"
  else
    func_name = string.format("function F_%s_%s:", parsed["parent"], parsed["id"])
  end

  output:write(string.format("\n%s\n", func_name))
end

-- output a string to a file
function write_string(output, parsed)
  output:write(string.format("\""))
  for i=1,parsed["str_len"]-1 do
    char = string.byte(parsed["value"],i)
    if char == 34 then output:write(string.format("\\\""))
    elseif char == 92 then output:write(string.format("\\\\"))
    elseif char == 7 then output:write(string.format("\\a"))
    elseif char == 8 then output:write(string.format("\\b"))
    elseif char == 12 then output:write(string.format("\\f"))
    elseif char == 10 then output:write(string.format("\\n"))
    elseif char == 13 then output:write(string.format("\\r"))
    elseif char == 9 then output:write(string.format("\\t"))
    elseif char == 11 then output:write(string.format("\\v"))
    else
      if isprint(char) then
        output:write(string.format("%c", char))
      else
        output:write(string.format("\\%d", char))
      end
    end
  end
  output:write(string.format("\""))
end

-- output a constant to a file
function write_constant(output, parsed, n)
  c_type = parsed["constant"][n]["c_type"]
  value = parsed["constant"][n]["value"]
  if c_type == types.LUA_TNIL or c_type == types.LUA_TBOOLEAN or c_type == types.LUA_TNUMBER then
    output:write(string.format("%s", value))
  elseif c_type == types.LUA_TSTRING then
    write_string(output, parsed["constant"][n])
  end
end

-- output a boolean to a file
function write_boolean(output, b)
  if b == 0 then
    output:write(string.format("FALSE"))
  else
    output:write(string.format("TRUE"))
  end
end

-- output a register to a file
function write_register(output, i)
  output:write(string.format("R[%s]", i))
end

-- output a constant or register to a file
function write_rk(output, parsed, x)
  if isk(x) then
    write_constant(output, parsed, indexk(x+1))
  else
    write_register(output, x)
  end
end

-- output an index to a file
function write_index(output, x)
  if isk(x) then
    output:write(string.format("%s", -1-indexk(x)))
  else
    output:write(string.format("%s", x))
  end
end

-- output function id to a file
function write_function_id(output, parsed, i)
  local parent = parsed["function"][i]["parent"]
  local id = parsed["function"][i]["id"]
  local np = parsed["function"][i]["number_of_parameters"]
  output:write(string.format("F_%s_%s %d", parent, id, np))
end

-- output an space to a file
function write_space(output)
  output:write(string.format(" "))
end

-- output instruction to a file
function write_code(output, parsed)
  for n=1,parsed["number_of_instructions"] do
    i = parsed["instruction"][n]
    o = get_opcode(i) + 1
    a = getarg_a(i)
    b = getarg_b(i)
    c = getarg_c(i)
    bx = getarg_bx(i)
    sbx = getarg_sbx(i)
    if parsed["number_of_instruction_lines"] > 0 then line = parsed["instruction_line"][n] else line = 0 end

    output:write(string.format("\t%d\t", n))
    if line > 0 then
      output:write(string.format("[%d]\t", line))
    else
      output:write(string.format("[0]\t"))
    end
    output:write(string.format("%-9s\t", opcode[o]))

    if opcode[o] ~= "JMP" then
      write_register(output, a)
    else
      output:write(string.format("%s", sbx+n+1))
    end

    if opcode[o] == "MOVE" or opcode[o] == "LOADNIL" or
      opcode[o] == "UNM" or opcode[o] == "NOT" or opcode[o] == "LEN" then
      write_space(output)
      write_register(output, b)
    elseif opcode[o] == "LOADK" then
      write_space(output)
      write_constant(output, parsed, bx+1)
    elseif opcode[o] == "LOADBOOL" then
      write_space(output)
      write_boolean(output, b)
      write_space(output)
      write_index(output, c)
    elseif opcode[o] == "GETUPVAL" or opcode[o] == "SETUPVAL" then
      write_space(output)
      output:write(string.format("%s", parsed["upvalue"][b+1]["value"]))
    elseif opcode[o] == "GETGLOBAL" or opcode[o] == "SETGLOBAL" then
      write_space(output)
      output:write(string.format("%s", parsed["constant"][bx+1]["value"]))
    elseif opcode[o] == "GETTABLE" or opcode[o] == "SELF" then
      write_space(output)
      write_register(output, b)
      write_space(output)
      write_rk(output, parsed, c)
    elseif opcode[o] == "SETTABLE" or
           opcode[o] == "ADD" or opcode[o] == "SUB" or opcode[o] == "MUL" or
           opcode[o] == "DIV" or opcode[o] == "MOD" or opcode[o] == "POW" or
           opcode[o] == "EQ" or opcode[o] == "LT" or opcode[o] == "LE" then
      write_space(output)
      write_rk(output, parsed, b)
      write_space(output)
      write_rk(output, parsed, c)
    elseif opcode[o] == "NEWTABLE" or opcode[o] == "CALL" or opcode[o] == "TAILCALL" or
           opcode[o] == "SETLIST" then
      write_space(output)
      write_index(output, b)
      write_space(output)
      write_index(output, c)
    elseif opcode[o] == "CONCAT" then
      write_space(output)
      write_register(output, b)
      write_space(output)
      write_register(output, c)
    elseif opcode[o] == "FORLOOP" or opcode[o] == "FORPREP" then
      write_space(output)
      output:write(string.format("%s", sbx+n+1))
    elseif opcode[o] == "TEST" or opcode[o] == "TESTSET" then
      write_space(output)
      write_register(output, b)
      write_space(output)
      write_boolean(output, c)
    elseif opcode[o] == "RETURN" or opcode[o] == "VARARG" then
      write_space(output)
      write_index(output, b)
    elseif opcode[o] == "TFORLOOP" then
      write_space(output)
      write_index(output, c)
    elseif opcode[o] == "CLOSE" then
    elseif opcode[o] == "CLOSURE" then
      write_space(output)
      write_function_id(output, parsed, bx+1)
    end

    output:write(string.format("\n"))
  end

end

-- output a function to a file
function write_function(output, parsed)
  write_header(output, parsed)
  write_code(output, parsed)

  for i=1,parsed["number_of_functions"] do
    write_function(output, parsed["function"][i])
  end
end

if #arg < 1 then
  usage("no input file given");
end

doargs()

if options.INPUT == nil then
  usage("no input file given")
end

if not (loadfile(options.INPUT)) then
  usage(options.INPUT .. " can not be loaded")
end

contents = string.dump(loadfile(options.INPUT))

tmp = assert(io.open(options.TMP, "wb"))
tmp:write(contents)
tmp:close()

input = assert(io.open(options.TMP, "rb"))

if not input then
  io.write(string.format("Couldn't open %s!\n", options.TMP))
end

header_chunk = load_header(input)

-- check if Lua version is 5.1
if not (check_version(header_chunk)) then
  io.write(string.format("Can't proceed. It works only with Lua 5.1.\n"))
  input:close()
  os.exit(1)
end

-- set flags got in the input file
set_auto_flags(header_chunk)

if not parsed["main"] then parsed["main"] = {} end

-- check source name
source_name = get_source_name(input, parsed["main"])

-- scan input file
parsed["main"]["id"] = 0
get_function_block(input, parsed["main"], source_name, parsed["main"]["id"])

input:close()
os.remove(options.TMP)

if options.LISTING then print_function(parsed["main"]) end

output = assert(io.open(options.OUTPUT, "w"))

if not output then
  io.write(string.format("Couldn't open %s!\n", options.OUTPUT))
end

write_function(output, parsed["main"])

output:close()

os.exit(0)
