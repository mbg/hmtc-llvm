@echo off

set LLVM_PATH=D:\SDKs\llvm.mingw
set GHC_LIBS=-lLLVMLinker -lLLVMipo -lLLVMInterpreter -lLLVMInstrumentation -lLLVMJIT -lLLVMExecutionEngine -lLLVMBitWriter -lLLVMX86AsmParser -lLLVMX86AsmPrinter -lLLVMX86CodeGen -lLLVMX86Info -lLLVMAsmParser -lLLVMArchive -lLLVMBitReader -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMCodeGen -lLLVMScalarOpts -lLLVMTransformUtils -lLLVMipa -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMCore -lLLVMSupport -lLLVMMC -lLLVMMCParser -lpsapi -limagehlp -lstdc++

set HMTC_SOURCE_FILES=LLVM\Base\FFI\llvm.c Main.hs

echo Preprocessing files...

hsc2hs LLVM\Base\FFI\Core.hsc -I%LLVM_PATH%\include
happy Parser.y -o Parser.hs

echo Compiling...

ghc -o HMTC.exe -pgml gcc %HMTC_SOURCE_FILES% -I%LLVM_PATH%\include %GHC_LIBS% -L%LLVM_PATH%\lib

rem del /S *.o