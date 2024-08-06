#include "VRadix2Divider.h"
#include "verilated.h"
int main(int argc, char **argv)
{
    VerilatedContext *contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);
    VRadix2Divider *top = new VRadix2Divider{contextp};
    top->clock = 0;
    top->reset = 1;
    top->eval();
    contextp->timeInc(1);
    top->reset = 0;
    delete top;
    delete contextp;
    return 0;
}
