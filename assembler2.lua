local bit = require("bit")
local band, rshift, lshift = bit.band, bit.rshift, bit.lshift
local abs = math.abs

local inputPath, outputPath = ...

--== CONSTANTS ==--

local registers = {
 A       = 0,
 B       = 1,
 C       = 2,
 T       = 3,
 SP      = 4,
 VP      = 5,
 PP      = 6,
 FL      = 7,
}

local operandTypes = {
 A       = 0,
 B       = 1,
 C       = 2,
 T       = 3,
 SP      = 4,
 VP      = 5,
 PP      = 6,
 FL      = 7,
 Literal = 8,
 FromMem = 9,
}

local assemblerInstructions = {
 MARK  = 0,
 DATA  = 1,
 DSTR  = 2,
 CONST = 3,
}

local operationTypes = {
 HALT  = 0,
 NOP   = 1,
 RET   = 2,
 NEG   = 3,
 NOT   = 4,
 PUSH  = 5,
 POP   = 6,
 VPUSH = 7,
 VPOP  = 8,
 CALL  = 9,
 JMP   = 10,
 JG    = 11,
 JNG   = 12,
 JL    = 13,
 JNL   = 14,
 JE    = 15,
 JNE   = 16,
 EXTI  = 17,
 ADD   = 18,
 SUB   = 19,
 MUL   = 20,
 DIV   = 21,
 MOD   = 22,
 SMUL  = 23,
 SDIV  = 24,
 SMOD  = 25,
 AND   = 26,
 OR    = 27,
 XOR   = 28,
 SHL   = 29,
 SHR   = 30,
 SAR   = 31,
 SET   = 32,
 GET   = 33,
 SWAP  = 34,
 CMP   = 35,
}

--== DATA ==--

local replaceLabels = {}
local constants = {}
local shortData = {}

local lineNum = 0
local sdPointer = 0

--== FUNCTIONS ==--

local function shortWrite(value)
 shortData[sdPointer] = value
 print("Wrote "..value.." at "..sdPointer)
 sdPointer = sdPointer + 1
end


local function encodeOperation(opType, op1, op2)
 if not type(opType) == "number" then
  print("Line"..lineNum..", encodeOperation: first parameter opType must be number, got "..(opType or "nil"))
  os.exit(0)
 end
 
 local opCode
 
 if opType <= 2 then --No operands
  return opType
  
 elseif opType <= 17 then --One operand
  opCode = opType * 10 - 27 + op1.type --Algebra'd. The original equation is   (opType - 3) * 10 + 3 + op1.type
 
 elseif opType <= 35 then --Two operands
  opCode = opType * 90 - 1467 + op1.type + op2.type * 10 --Algebra'd. The original equation is   (opType - 18) * 90 + 153 + op1.type + op2.type * 10
 else
  print("Line"..lineNum..", invalid operand type "..opType)
  os.exit(0)
 end
 
 return opCode
end

local function encodeFromMemory(register, register2, reg2sign, offset)
 if register2 then
  print("I haven't implemented multiple-register FromMem reads, yet.")
  os.exit(0)
 else
  local operand
  
  if offset then
   if offset < 0 then
    print("I haven't implemented negative inline FromMem offsets, yet.")
    os.exit(0)
   else
    operand = register + lshift(offset, 4) 
   end
  else
   operand = register
  end
  
  return operand
 end
end


local function parseOperand(operand)
 local ret
 
 local opType = registers[operand]
 if opType then -- This is just a reference to a register. No need to get fancy.
  ret = {type = opType}
 else -- It's not a register.
  local inBrackets = operand:match("%[(.*)%]")
  
  if inBrackets then --Try and read it as a FromMem operation
   local offsetIndex = inBrackets:find("[%+%-]")
   local offset
   
   local registerName
   local offsetIndex2
   local register2
   
   if offsetIndex then --Something else is offsetting this register.
    offset = tonumber(inBrackets:sub(offsetIndex))
    
    if not offset then --Likely a second register
     offsetIndex2 = inBrackets:find("[%+%-]", offsetIndex);
     register2 = inBrackets:sub(offsetIndex + 1, offsetIndex2 - 1);
     offset = tonumber(inBrackets:sub(offsetIndex2))
     
     if not offset then
      print("Line "..lineNum..", Unknown from-memory configuration: "..inBrackets)
      os.exit(0)
     end
    end
    
    registerName = inBrackets:sub(0, offsetIndex - 1)
   else --There's no offset at all.
    registerName = inBrackets
   end
   
   register = registers[registerName]
   if not register then
    print("Line "..lineNum..", Invalid register "..registerName)
    os.exit(0)
   end
   
   if register2 then
    register2 = registers[register2]
    local reg2Sign = inBrackets:sub(offsetIndex, offsetIndex) == "-" and 1 or 0
    ret = {type = 9, value = encodeFromMemory(register, register2, reg2Sign, offset)} -- FromMem
   else
    ret = {type = 9, value = encodeFromMemory(register, nil, nil, offset)} -- FromMem
   end
    
  else --It's not a register, and it's not a FromMem. We'll just treat it as a label.
   local literalValue = tonumber(operand)
   ret = {type = 8, value = literalValue or operand} -- Literal
  end
 end
 
 return ret
end


local function parseLine(ln)
 print (ln)
 -- Scrub out comments
 local commentStart = ln:find(";")
 if commentStart then
  ln = ln:sub(0, commentStart - 1)
 end
 
 -- Find and split every relevant string
 local symbols = {}
 for symbol in ln:gmatch("[^%s,]+") do
  table.insert(symbols, symbol)
 end
 
 local operation = symbols[1] -- The operation to be performed
 
 if operation then -- Checks to see if there's anything actually on this line.
 
  local opType = operationTypes[operation]
  if opType then -- This could be a processor instruction.
   local op1, op2 = symbols[2], symbols[3] -- The operands.
   
   if op1 then
    op1 = parseOperand(op1)
   end
   if op2 then
    op2 = parseOperand(op2)
   end
   
   local opCode = encodeOperation(opType, op1, op2)
   shortWrite(opCode)
   
   if op1 and op1.value then
    if type(op1.value) == "string" then
     replaceLabels[sdPointer] = op1.value
    end
    
    shortWrite(op1.value)
   end
   
   if op2 and op2.value then
    if op2.type == 9 then -- FromMem
     print("Line "..lineNum..", second operand cannot be from memory.")
     os.exit(0)
    end
    
    if type(op2.value) == "string" then
     replaceLabels[sdPointer] = op2.value
    end
    shortWrite(op2.value)
   end
   
  else -- It's not a processor instruction. Maybe it's an assembler instruction.
   local instruction = assemblerInstructions[operation]
   if instruction then
    if instruction == 0 then -- MARK
     if not symbols[2] then
      print("Line "..lineNum..", MARK requires a label")
      os.exit(0)
     end
     constants[symbols[2]] = sdPointer
     print("Marked "..sdPointer.." as "..symbols[2])
     
    elseif instruction == 1 then -- DATA
     if not symbols[2] then
      print("Line "..lineNum..", empty DATA line")
     end
     
     for i = 2, #symbols do
      shortWrite(symbols[i])
     end
     
    elseif instruction == 2 then -- DSTR
    
    elseif instruction == 3 then -- CONST
     if not symbols[2] then
      print("Line "..lineNum..", CONST requires a label")
      os.exit(0)
     end
     if not symbols[3] then
      print("Line "..lineNum..", CONST requires a value")
      os.exit(0)
     end
     
     local value = tonumber(symbols[3])
     
     if not value then
      print("Line "..lineNum..", expected number, got "..symbols[3])
      os.exit(0)
     end
     
     constants[symbols[2]] = value
     
     print("Defined constant "..symbols[2].." as "..value)
    end
   else -- No idea what it is. Definitely not valid syntax.
    print("Line "..lineNum..", unexpected symbol "..operation)
    os.exit(0)
   end
  end
 end
end


--== ASSEMBLER ==--

local inputFile = assert(io.open(inputPath, "r"))

for ln in inputFile:lines() do
 parseLine(ln)
 lineNum = lineNum + 1
end

for k,v in pairs(replaceLabels) do
 if not shortData[k] then
  print("Something has gone horribly, horribly wrong.")
  os.exit(0)
 end
 if not constants[v] then
  print("Could not find constant "..v)
  os.exit(0)
 end
 shortData[k] = constants[v]
 print("Replaced label "..v.." at "..k.." with address "..constants[v])
end

local outputFile = assert(io.open(outputPath, "wb"))

for i = 0, #shortData do
 --print("Writing: "..shortData[i])
 local mSigByte = rshift(shortData[i], 8)
 local lSigByte = band(shortData[i], 0xFF)
 outputFile:write(string.char(mSigByte))
 outputFile:write(string.char(lSigByte))
end

outputFile:close()
