var searchIndex = JSON.parse('{\
"asm_19":{"doc":"A virtual ASM-19 chip to be used with emulators.","i":[[0,"memory","asm_19","",null,null],[8,"Memory","asm_19::memory","The Processor expects to be given a struct implementing …",null,null],[11,"read","","This method should be overriden by the implementor of …",0,[[["u16",15]],[["result",4],["memoryreaderror",3],["u16",15]]]],[11,"write","","This method should be overriden by the implementor of …",0,[[["u16",15]],[["result",4],["memorywriteerror",3]]]],[3,"MemoryReadError","","Created when a struct implementing the Memory trait is …",null,null],[12,"message","","",1,null],[12,"address","","",1,null],[3,"MemoryWriteError","","Created when a struct implementing the Memory trait is …",null,null],[12,"message","","",2,null],[12,"address","","",2,null],[12,"value","","",2,null],[5,"merge_16","","Merges the first two indices of the provided slice into a …",null,[[],["u16",15]]],[0,"processor","asm_19","",null,null],[3,"Processor","asm_19::processor","An emulated ASM-19 processor.",null,null],[11,"new","","Creates a new, unhalted processor with all registers …",3,[[],["processor",3]]],[11,"tick","","Causes the CPU to execute the instruction in memory at …",3,[[["bool",15],["memory",8]]]],[11,"from","asm_19::memory","",1,[[]]],[11,"into","","",1,[[]]],[11,"borrow","","",1,[[]]],[11,"borrow_mut","","",1,[[]]],[11,"try_from","","",1,[[],["result",4]]],[11,"try_into","","",1,[[],["result",4]]],[11,"type_id","","",1,[[],["typeid",3]]],[11,"from","","",2,[[]]],[11,"into","","",2,[[]]],[11,"borrow","","",2,[[]]],[11,"borrow_mut","","",2,[[]]],[11,"try_from","","",2,[[],["result",4]]],[11,"try_into","","",2,[[],["result",4]]],[11,"type_id","","",2,[[],["typeid",3]]],[11,"from","asm_19::processor","",3,[[]]],[11,"into","","",3,[[]]],[11,"to_string","","",3,[[],["string",3]]],[11,"borrow","","",3,[[]]],[11,"borrow_mut","","",3,[[]]],[11,"try_from","","",3,[[],["result",4]]],[11,"try_into","","",3,[[],["result",4]]],[11,"type_id","","",3,[[],["typeid",3]]],[11,"fmt","","",3,[[["formatter",3]],["result",6]]]],"p":[[8,"Memory"],[3,"MemoryReadError"],[3,"MemoryWriteError"],[3,"Processor"]]}\
}');
addSearchOptions(searchIndex);initSearch(searchIndex);