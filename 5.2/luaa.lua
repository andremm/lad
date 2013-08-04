#!/usr/bin/env lua

--[[
Lua Assembler for Lua 5.2.0
Version 0.2
Author: Andre Murbach Maidl
]]

INPUT = "luad.asm"
OUTPUT = "luaa.out"
DEBUG = false

USAGE = [[
usage: %s [options] [filename]
Available options are:
-h      print this help
-o name output to file name (default is %s)
]]

function usage(msg)
  if msg ~= nil then
    io.write(string.format("%s: %s\n", arg[0], msg))
  end
  io.write(string.format(USAGE, arg[0], OUTPUT))
  os.exit(1)
end

function doargs()
  local i = 1
  while i <= #arg do
    if string.find(arg[i], "^-") == nil then
      INPUT = arg[i]
      break
    elseif arg[i] == "-h" then usage()
    elseif arg[i] == "-o" then i = i + 1
      if arg[i] == nil then usage("'-o' needs argument") else OUTPUT = arg[i] end
    else usage(string.format("'%s' unkown option", arg[i]))
    end
    i = i + 1
  end
end

function read_input(filename)
  local input = assert(io.open(filename, "r"))
  local contents = input:read("*a")
  input:close()
  return contents
end

if #arg < 1 then
  usage ("no input file given")
end

doargs()

local assembler = require("assembler")
local contents = read_input(INPUT)
local ast = assembler.parse(contents)

if not ast then
  print ("syntax error")
  os.exit (1)
end

if DEBUG then assembler.print_ast(ast) end
local parsed = assembler.traverse(ast)
if parsed == nil then
  error ("could not generate bytecode")
end
assembler.write(OUTPUT, parsed)

os.exit(0)
