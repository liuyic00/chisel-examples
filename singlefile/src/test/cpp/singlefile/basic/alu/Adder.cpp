#include "VAdder.h"
#include "verilated.h"
int main(int argc, char **argv)
{
    VerilatedContext *contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);
    VAdder *top = new VAdder{contextp};

    top->io_valid = 1;

    top->io_in1 = 1;
    top->io_in2 = 1;
    top->eval();
    printf("top->io_out = %u : 2\n", top->io_out);
    top->io_in1 = 3;
    top->io_in2 = 4;
    top->eval();
    printf("top->io_out = %u : 7\n", top->io_out);

    contextp->timeInc(1);
    top->eval();
    printf("top->io_out = %u : 7\n", top->io_out);

    top->io_valid = 0;
    top->eval();
    printf("top->io_out = %u : 0\n", top->io_out);

    delete top;
    delete contextp;
    return 0;
}
