#include "VArrayMulDataModule.h"
#include "verilated.h"
#include "verilated_vcd_c.h"

void clock_up(VerilatedContext *contextp,
              VerilatedVcdC *tracep,
              VArrayMulDataModule *top)
{
    top->clock = 1;
    top->eval();
    tracep->dump(contextp->time());
    contextp->timeInc(1);
}
void clock_down(VerilatedContext *contextp,
                VerilatedVcdC *tracep,
                VArrayMulDataModule *top)
{
    top->clock = 0;
    top->eval();
    tracep->dump(contextp->time());
    contextp->timeInc(1);
}

int main(int argc, char **argv)
{
    VerilatedContext *contextp = new VerilatedContext;
    VerilatedVcdC *tracep = new VerilatedVcdC;
    VArrayMulDataModule *top = new VArrayMulDataModule{contextp};

    contextp->commandArgs(argc, argv);
    contextp->traceEverOn(true);
    top->trace(tracep, 0);
    tracep->open("test_run_dir/verilator/ArrayMulDataModule.vcd");

    int len = 64;

    VlWide<5UL> result;

    // init
    top->reset = 1;

    clock_down(contextp, tracep, top);

    top->reset = 0;
    top->io_a = VlWide<3UL>{2, 0, 0};
    top->io_b = VlWide<3UL>{3, 0, 0};
    top->io_regEnables_0 = 1;
    top->io_regEnables_1 = 1;

    clock_up(contextp, tracep, top);
    clock_down(contextp, tracep, top);
    if (len >= 64)
    {
        clock_up(contextp, tracep, top);
        clock_down(contextp, tracep, top);
    }

    printf("top->io_result = %u : 6\n", top->io_result.at(0));

    tracep->close();

    delete top;
    delete contextp;
    return 0;
}
