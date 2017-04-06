#include <stdio.h>
#include <stdlib.h>
#include "bryantPrint.h"
#include "bryant.h"


int print_bryant(FILE *stream, node *f, bitset *trues, bitset *falses,
                 int terms);


/* Print out an ROBDD in some readable format.  We display it in disjunctive
 * form.
 */

void printOut(FILE *stream, node *f)
    {
	bitset trues, falses;

	if (f == one) {
            fprintf(stream, "TRUE");
	} else if (f == zero) {
	    fprintf(stream, "FALSE");
	} else {
	    BITSET_CLEAR(trues);
	    BITSET_CLEAR(falses);
	    (void)print_bryant(stream, f, &trues, &falses, 0);
	}
    }


int print_bryant(FILE *stream, node *f,
                 bitset *trues, bitset *falses, int terms)
    {
	if (f == one) {
	    bitset all;
	    int var;
	    int word;
	    bitmask mask;
	    char sep = '(';

	    if (terms>0) fprintf(stream, " ");
	    BITSET_UNION(all, *trues, *falses);
	    FOREACH_ELEMENT(all, var, word, mask) {
		if (BITSET_MEMBER(*trues, word, mask)) {
		    fprintf(stream, "%c%d", sep, var);
		} else {
		    fprintf(stream, "%c~%d", sep, var);
		}
		sep = ' ';
	    }
	    fprintf(stream, ")");
	    ++terms;
	} else if (f != zero) {
	    BITSET_ADD_ELEMENT(*trues, f->value);
	    terms += print_bryant(stream, f->tr, trues, falses, terms);
	    BITSET_TOGGLE_ELEMENT(*trues, f->value);
	    BITSET_ADD_ELEMENT(*falses, f->value);
	    terms += print_bryant(stream, f->fa, trues, falses, terms);
	    BITSET_TOGGLE_ELEMENT(*falses, f->value);
	}
	/* don't do anything for zero terminal */
	return terms;
    }
