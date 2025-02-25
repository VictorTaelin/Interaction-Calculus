#include <stdio.h>
#include "show.h"
#include "memory.h"

// Get the string representation of a term tag
const char* show_tag(TermTag tag) {
  switch (tag) {
    case VAR: return "VAR";
    case LET: return "LET";
    case SUP: return "SUP";
    case CO0: return "CO0";
    case CO1: return "CO1";
    case SET: return "SET";
    case EMP: return "EMP";
    case EFQ: return "EFQ";
    case UNI: return "UNI";
    case NIL: return "NIL";
    case USE: return "USE";
    case BIT: return "BIT";
    case B_0: return "B_0";
    case B_1: return "B_1";
    case ITE: return "ITE";
    case SIG: return "SIG";
    case TUP: return "TUP";
    case GET: return "GET";
    case ALL: return "ALL";
    case LAM: return "LAM";
    case APP: return "APP";
    case EQL: return "EQL";
    case RFL: return "RFL";
    case RWT: return "RWT";
    default: return "UNKNOWN";
  }
}

// Convert a term to string representation and write to the given file
void show_term(FILE* file, Term term) {
  TermTag tag = TERM_TAG(term);
  uint8_t lab = TERM_LAB(term);
  uint32_t val = TERM_VAL(term);

  switch (tag) {
    case VAR:
      fprintf(file, "x%u", val);
      break;
    case LET:
      fprintf(file, "!");
      show_term(file, heap[val]);
      fprintf(file, ";");
      show_term(file, heap[val + 1]);
      break;
    case SUP:
      fprintf(file, "&%u{", lab);
      show_term(file, heap[val]);
      fprintf(file, ",");
      show_term(file, heap[val + 1]);
      fprintf(file, "}");
      break;
    case CO0:
    case CO1:
      fprintf(file, "c%u_%u", tag == CO0 ? 0 : 1, val);
      break;
    case SET:
      fprintf(file, "*");
      break;
    case EMP:
      fprintf(file, "⊥");
      break;
    case EFQ:
      fprintf(file, "¬");
      show_term(file, heap[val]);
      break;
    case UNI:
      fprintf(file, "⊤");
      break;
    case NIL:
      fprintf(file, "()");
      break;
    case USE:
      fprintf(file, "-");
      show_term(file, heap[val]);
      fprintf(file, ";");
      show_term(file, heap[val + 1]);
      break;
    case BIT:
      fprintf(file, "𝔹");
      break;
    case B_0:
      fprintf(file, "0");
      break;
    case B_1:
      fprintf(file, "1");
      break;
    case ITE:
      fprintf(file, "?");
      show_term(file, heap[val]);
      fprintf(file, "{");
      show_term(file, heap[val + 1]);
      fprintf(file, "};{");
      show_term(file, heap[val + 2]);
      fprintf(file, "}");
      break;
    case SIG:
      fprintf(file, "Σ");
      show_term(file, heap[val]);
      fprintf(file, ".");
      show_term(file, heap[val + 1]);
      break;
    case TUP:
      fprintf(file, "[");
      show_term(file, heap[val]);
      fprintf(file, ",");
      show_term(file, heap[val + 1]);
      fprintf(file, "]");
      break;
    case GET:
      fprintf(file, "![");
      show_term(file, heap[val]);
      fprintf(file, ",");
      show_term(file, heap[val + 1]);
      fprintf(file, "]=");
      show_term(file, heap[val + 2]);
      fprintf(file, ";");
      show_term(file, heap[val + 3]);
      break;
    case ALL:
      fprintf(file, "Π");
      show_term(file, heap[val]);
      fprintf(file, ".");
      show_term(file, heap[val + 1]);
      break;
    case LAM:
      fprintf(file, "λ");
      show_term(file, heap[val]);
      break;
    case APP:
      fprintf(file, "(");
      show_term(file, heap[val]);
      fprintf(file, " ");
      show_term(file, heap[val + 1]);
      fprintf(file, ")");
      break;
    case EQL:
      fprintf(file, "<");
      show_term(file, heap[val]);
      fprintf(file, "=");
      show_term(file, heap[val + 1]);
      fprintf(file, ">");
      break;
    case RFL:
      fprintf(file, "θ");
      break;
    case RWT:
      fprintf(file, "%%");
      show_term(file, heap[val]);
      fprintf(file, ";");
      show_term(file, heap[val + 1]);
      break;
    default:
      fprintf(file, "<?>");
      break;
  }
}