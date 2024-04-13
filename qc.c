#define SOL_DEF
#include "../solh/sol.h"

int main(int argc, const char *argv[]) {

    if (argc == 1) {
        println("qc usage:\n    qc file1.c file2.c ... -- opt1 opt2 ...");
        println("    options: -cl <gcc|clang>");
        return 0;
    }

    const char **optv = NULL;
    uint optc;
    for(uint i = 1; i < argc; ++i)
        if (strcmp(argv[i], "--") == 0) {
            optv = argv + i;
            optc = argc - i;
            break;
        }

    const char *clang = "clang-17";
    const char *gcc = "gcc";

    const char *cl = gcc;
    if (optv)
        for(uint i = 1; i < optc; ++i)
            if (!memcmp(optv[i], "-cl", 3)) {
                cl = !memcmp(optv[i+1], "clang", 5) ? clang : gcc;
                i++;
            }

    char *cmd = malloc(1024);
    string_format(cmd, "%s ", cl);

    // CBA to merge the conditional to one loop.
    for(uint i = 1; !optv && i < argc; ++i)
        string_format(cmd + strlen(cmd), "%s ", argv[i]);
    for(uint i = 1; optv && (argv + i != optv); ++i)
        string_format(cmd + strlen(cmd), "%s ", argv[i]);

    string_format(cmd + strlen(cmd), "-mbmi -mlzcnt -ggdb -lm -o qc");

    system(cmd);

    return 0;
}
