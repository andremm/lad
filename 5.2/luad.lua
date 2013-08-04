#!/usr/bin/env lua

--[[
Lua Disassembler for Lua 5.2.0
Version 0.2
Author: Andre Murbach Maidl
]]

INPUT = "luac.out"
OUTPUT = "luad.asm"
LISTING = 0
DUMPING = true

USAGE = [[
usage: %s [options] [filename]
Available options are:
-h      print this help
-l      list on luac style (use -l -l for full listing)
-o name output to file name (default is %s)
-p      parse only
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
    elseif arg[i] == "-l" then LISTING = LISTING + 1
    elseif arg[i] == "-o" then i = i + 1
      if arg[i] == nil then usage("'-o' needs argument") else OUTPUT = arg[i] end
    elseif arg[i] == "-p" then DUMPING = false
    else usage(string.format("'%s' unkown option", arg[i]))
    end
    i = i + 1
  end
end

if #arg < 1 then
  usage ("no input file given")
end

doargs()

local chunk = assert(loadfile(INPUT))
local bytecode = string.dump(chunk)
local disassembler = require("disassembler")

local parsed = disassembler.parse(bytecode)
if LISTING > 0 then disassembler.print_function(parsed, LISTING > 1) end
if DUMPING then disassembler.write(OUTPUT, parsed) end

os.exit(0)
