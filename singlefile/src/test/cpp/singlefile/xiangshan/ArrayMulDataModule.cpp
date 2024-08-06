#include "VArrayMulDataModule.h"
#include "verilated.h"
int main(int argc, char **argv)
{
    VerilatedContext *contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);
    VArrayMulDataModule *top = new VArrayMulDataModule{contextp};
    top->clock = 0;
    top->reset = 1;
    top->eval();
    top->reset = 0;
    delete top;
    delete contextp;
    return 0;
}
