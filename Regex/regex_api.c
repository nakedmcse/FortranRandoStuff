// C API for regex
#include <regex.h>
#include <stdlib.h>

void api_alloc(regex_t **reg_return) {
    *reg_return = malloc(sizeof(regex_t));
}

void api_regcomp(regex_t *preg, const char *pattern, const char *flags, int *status) {
    int cflags=0;
    for (int i=0;flags[i];i++) {
        switch (flags[i]) {
            case 'i': cflags |= REG_ICASE; break;
            case 'm': cflags |= REG_NEWLINE; break;
            case 'x': cflags |= REG_EXTENDED; break;
            case 'n': cflags |= REG_NOSUB; break;
            case ' ': break;
            default: *status=-2; return;
        }
    }
    *status = regcomp(preg,pattern,cflags);
}

void api_regexec(const regex_t *preg, const char *string, int nmatch, int matches[nmatch][2], const char *flags,
               int *status) {
    int eflags=0;
    const char *p = string;
    regmatch_t *pmatch;
    for (int i=0;flags[i];i++) {
        switch (flags[i]) {
            case 'b': eflags |= REG_NOTBOL; break;
            case 'e': eflags |= REG_NOTEOL; break;
            case ' ': break;
            default: *status=-2; return;
        }
    }

    pmatch = malloc(sizeof(regmatch_t)*nmatch);
    for(int j=0;j<nmatch;j++) {
        *status = regexec(preg,p,1,pmatch,eflags);
        if (status[0] == REG_NOMATCH) break;
        regoff_t start = pmatch[0].rm_so + (p - string);
        regoff_t end = pmatch[0].rm_eo + (p - string);
        matches[j][0]=(int)start;
        matches[j][1]=(int)end;
        p += pmatch[0].rm_eo;
    }
    free(pmatch);
}