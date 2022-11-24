#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum { ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END } Kind;
typedef enum { STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR } GrammarState;
typedef struct TokenUnit {
    Kind kind;
    int val;    // record the integer value or variable name
    struct TokenUnit* next;
} Token;
typedef struct ASTUnit {
    Kind kind;
    int val;    // record the integer value or variable name
    struct ASTUnit *lhs, *mid, *rhs;
} AST;
typedef struct result_linked_list {
    char* value;
    struct result_linked_list* next;
} Result;
/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x)                                                                                                                                                                                         \
    {                                                                                                                                                                                                  \
        puts("Compile Error!");                                                                                                                                                                        \
        if (DEBUG) {                                                                                                                                                                                   \
            fprintf(stderr, "Error at line: %d\n", __LINE__);                                                                                                                                          \
            fprintf(stderr, "Error message: %s\n", x);                                                                                                                                                 \
        }                                                                                                                                                                                              \
        exit(0);                                                                                                                                                                                       \
    }
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 1
// Split the input char array into token linked list.
Token* lexer(const char* in);
// Create a new token.
Token* new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token** head);
// Parse the token array. Return the constructed AST.
AST* parser(Token* arr, size_t len);
// Parse the token array. Return the constructed AST.
AST* parse(Token* arr, int l, int r, GrammarState S);
// Create a new AST node.
AST* new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token* arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST* now);
// combine constant
void ast_opt(AST* root);
// Generate ASM code.
int codegen(AST* root, Kind parent_kind);
// Free the whole AST.
void freeAST(AST* now);

/// debug interfaces

// Print token array.
void token_print(Token* in, size_t len);
// Print AST tree.
void AST_print(AST* head);

char input[MAX_LENGTH];

AST* global_root = NULL;    /////////delete if not debugging/////////
int main() {
    while (fgets(input, MAX_LENGTH, stdin) != NULL) {
        Token* content = lexer(input);
        size_t len = token_list_to_arr(&content);
        if (len == 0) continue;
        token_print(content, len);    /////////delete if not debugging/////////
        AST* ast_root = parser(content, len);
        global_root = ast_root;    /////////delete if not debugging/////////
        semantic_check(ast_root);
        AST_print(ast_root);    /////////delete if not debugging/////////
        ast_opt(ast_root);
        codegen(ast_root, END);
        free(content);
        freeAST(ast_root);
    }
    return 0;
}

Token* lexer(const char* in) {
    Token* head = NULL;
    Token** now = &head;
    for (int i = 0; in[i]; i++) {
        if (isspace(in[i]))    // ignore space characters
            continue;
        else if (isdigit(in[i])) {
            (*now) = new_token(CONSTANT, atoi(in + i));
            while (in[i + 1] && isdigit(in[i + 1]))
                i++;
        } else if ('x' <= in[i] && in[i] <= 'z')    // variable
            (*now) = new_token(IDENTIFIER, in[i]);
        else switch (in[i]) {
                case '=':
                    (*now) = new_token(ASSIGN, 0);
                    break;
                case '+':
                    if (in[i + 1] && in[i + 1] == '+') {
                        i++;
                        // In lexer scope, all "++" will be labeled as PREINC.
                        (*now) = new_token(PREINC, 0);
                    }
                    // In lexer scope, all single "+" will be labeled as PLUS.
                    else
                        (*now) = new_token(PLUS, 0);
                    break;
                case '-':
                    if (in[i + 1] && in[i + 1] == '-') {
                        i++;
                        // In lexer scope, all "--" will be labeled as PREDEC.
                        (*now) = new_token(PREDEC, 0);
                    }
                    // In lexer scope, all single "-" will be labeled as MINUS.
                    else
                        (*now) = new_token(MINUS, 0);
                    break;
                case '*':
                    (*now) = new_token(MUL, 0);
                    break;
                case '/':
                    (*now) = new_token(DIV, 0);
                    break;
                case '%':
                    (*now) = new_token(REM, 0);
                    break;
                case '(':
                    (*now) = new_token(LPAR, 0);
                    break;
                case ')':
                    (*now) = new_token(RPAR, 0);
                    break;
                case ';':
                    (*now) = new_token(END, 0);
                    break;
                default:
                    err("Unexpected character.");
            }
        now = &((*now)->next);
    }
    return head;
}

Token* new_token(Kind kind, int val) {
    Token* res = (Token*)malloc(sizeof(Token));
    res->kind = kind;
    res->val = val;
    res->next = NULL;
    return res;
}

size_t token_list_to_arr(Token** head) {
    size_t res;
    Token *now = (*head), *del;
    for (res = 0; now != NULL; res++)
        now = now->next;
    now = (*head);
    if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
    for (int i = 0; i < res; i++) {
        (*head)[i] = (*now);
        del = now;
        now = now->next;
        free(del);
    }
    return res;
}

AST* parser(Token* arr, size_t len) {
    for (int i = 1; i < len; i++) {
        // correctly identify "ADD" and "SUB"
        if (arr[i].kind == PLUS || arr[i].kind == MINUS) {
            switch (arr[i - 1].kind) {
                case PREINC:
                case PREDEC:
                case IDENTIFIER:
                case CONSTANT:
                case RPAR:
                    arr[i].kind = arr[i].kind - PLUS + ADD;
                default:
                    break;
            }
        }
    }
    return parse(arr, 0, len - 1, STMT);
}

AST* parse(Token* arr, int l, int r, GrammarState S) {
    // printf("#l=%d, r=%d, S=%d\n", l, r, S);
    AST* now = NULL;
    if (l > r) err("Unexpected parsing range.");
    int nxt;
    switch (S) {
        case STMT:
            if (l == r && arr[l].kind == END) return NULL;
            else if (arr[r].kind == END) return parse(arr, l, r - 1, EXPR);
            else err("Expected \';\' at the end of line.");
        case EXPR:
            return parse(arr, l, r, ASSIGN_EXPR);
        case ASSIGN_EXPR:
            if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1) {
                now = new_AST(arr[nxt].kind, 0);
                now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
                now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
                return now;
            }
            return parse(arr, l, r, ADD_EXPR);
        case ADD_EXPR:
            if ((nxt = findNextSection(arr, r, l, condADD)) != -1) {
                now = new_AST(arr[nxt].kind, 0);
                now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
                now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
                return now;
            }
            return parse(arr, l, r, MUL_EXPR);
        case MUL_EXPR:
            // TODO: Implement MUL_EXPR.
            // hint: Take ADD_EXPR as reference.
            if ((nxt = findNextSection(arr, r, l, condMUL)) != -1) {
                now = new_AST(arr[nxt].kind, 0);
                now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
                now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
                return now;
            }
            return parse(arr, l, r, UNARY_EXPR);
        case UNARY_EXPR:
            // TODO: Implement UNARY_EXPR.
            // hint: Take POSTFIX_EXPR as reference.
            if (arr[l].kind == PREINC || arr[l].kind == PREDEC || arr[l].kind == PLUS || arr[l].kind == MINUS) {
                // translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
                now = new_AST(arr[l].kind, 0);
                now->mid = parse(arr, l + 1, r, UNARY_EXPR);
                return now;
            }
            return parse(arr, l, r, POSTFIX_EXPR);
        case POSTFIX_EXPR:
            if (arr[r].kind == PREINC || arr[r].kind == PREDEC) {
                // translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
                now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
                now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
                return now;
            }
            return parse(arr, l, r, PRI_EXPR);
        case PRI_EXPR:
            if (findNextSection(arr, l, r, condRPAR) == r) {
                now = new_AST(LPAR, 0);
                now->mid = parse(arr, l + 1, r - 1, EXPR);
                return now;
            }
            if (l == r) {
                if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT) return new_AST(arr[l].kind, arr[l].val);
                err("Unexpected token during parsing.");
            }
            err("No token left for parsing.");
        default:
            err("Unexpected grammar state.");
    }
}

AST* new_AST(Kind kind, int val) {
    AST* res = (AST*)malloc(sizeof(AST));
    res->kind = kind;
    res->val = val;
    res->lhs = res->mid = res->rhs = NULL;
    return res;
}

int findNextSection(Token* arr, int start, int end, int (*cond)(Kind)) {
    int par = 0;
    int d = (start < end) ? 1 : -1;
    for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
        if (arr[i].kind == LPAR) par++;
        if (arr[i].kind == RPAR) par--;
        if (par == 0 && cond(arr[i].kind) == 1) return i;
    }
    return -1;
}

int condASSIGN(Kind kind) {
    return kind == ASSIGN;
}

int condADD(Kind kind) {
    return kind == ADD || kind == SUB;
}

int condMUL(Kind kind) {
    return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind) {
    return kind == RPAR;
}

void semantic_check(AST* now) {
    if (now == NULL) return;
    // Left operand of '=' must be an identifier or identifier with one or more parentheses.
    if (now->kind == ASSIGN) {
        AST* tmp = now->lhs;
        while (tmp->kind == LPAR)
            tmp = tmp->mid;
        if (tmp->kind != IDENTIFIER) err("Lvalue is required as left operand of assignment.");
    }
    // Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
    // TODO: Implement the remaining semantic_check code.
    // hint: Follow the instruction above and ASSIGN-part code to implement.
    // hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
    if (now->kind == PREINC || now->kind == PREDEC || now->kind == POSTINC || now->kind == POSTDEC) {
        AST* tmp = now->mid;
        while (tmp->kind == LPAR)
            tmp = tmp->mid;
        if (tmp->kind != IDENTIFIER) err("Operand of INC/DEC must be an identifier or identifier with one or more parentheses.");
    }

    semantic_check(now->lhs);
    semantic_check(now->mid);
    semantic_check(now->rhs);
}

void ast_opt(AST* root) {
    if (root == NULL) return;
    ast_opt(root->lhs);
    ast_opt(root->mid);
    ast_opt(root->rhs);

    // optimize op_+-*/% lhs->const rhs->const
    if (root->lhs != NULL && root->rhs != NULL && root->lhs->kind == CONSTANT && root->rhs->kind == CONSTANT) {
        switch (root->kind) {
            case ADD:
                root->val = root->lhs->val + root->rhs->val;
                break;
            case SUB:
                root->val = root->lhs->val - root->rhs->val;
                break;
            case MUL:
                root->val = root->lhs->val * root->rhs->val;
                break;
            case DIV:
                root->val = root->lhs->val / root->rhs->val;
                break;
            case REM:
                root->val = root->lhs->val % root->rhs->val;
                break;
            default:
                break;
        }
        root->kind = CONSTANT;
        root->lhs = NULL;
        root->mid = NULL;
        root->rhs = NULL;
        AST_print(global_root);
    }

    // optimize op_( mid->const
    if (root->kind == LPAR && root->mid != NULL && root->mid->kind == CONSTANT) {
        root->kind = CONSTANT;
        root->val = root->mid->val;
        root->lhs = NULL;
        root->mid = NULL;
        root->rhs = NULL;
        AST_print(global_root);
    }

    // optimize op_+ op_- mid->const
    if ((root->kind == PLUS || root->kind == MINUS) && root->mid != NULL && root->mid->kind == CONSTANT) {
        root->val = ((root->kind == PLUS) ? 1 : -1) * root->mid->val;
        root->kind = CONSTANT;
        root->lhs = NULL;
        root->mid = NULL;
        root->rhs = NULL;
        AST_print(global_root);
    }

    // optimize op_* lhs->constant==0 or rhs->constant==0
    if (root->kind == MUL && ((root->lhs->kind == CONSTANT && root->lhs->val == 0) || (root->rhs->kind == CONSTANT && root->rhs->val == 0))) {
        root->val = 0;
        root->kind = CONSTANT;
        root->lhs = NULL;
        root->mid = NULL;
        root->rhs = NULL;
        AST_print(global_root);
    }
    return;
}

Result result_root;
void push_back() {
}
int r_in_use[256] = {0};    // 0 unused, 1 used, 2 used by indentifier
int identifier_reg_index[3] = {-1, -1, -1};
int identifier_post_delta[3] = {0};
int find_first_unused_r() {
    for (int i = 0; i < 256; i++)
        if (r_in_use[i] == 0) return i;
    return -1;
}
void use_reg_r(int r) {
    if (r == -1) return;
    r_in_use[r] = 1;
}
void free_reg(int r_l, int r_m, int r_r) {
    if (r_l >= 0 && r_in_use[r_l] != 2) r_in_use[r_l] = 0;
    if (r_m >= 0 && r_in_use[r_m] != 2) r_in_use[r_m] = 0;
    if (r_r >= 0 && r_in_use[r_r] != 2) r_in_use[r_r] = 0;
}
int load_identifier(int identifier) {
    if (identifier_reg_index[identifier] == -1) {
        int index_load_to_r = find_first_unused_r();
        printf("load r%d [%d]\n", index_load_to_r, identifier * 4);
        identifier_reg_index[identifier] = index_load_to_r;
        r_in_use[index_load_to_r] = 2;
        return index_load_to_r;
    }
    return -1;
}
int basic_op(const char* op_name, AST* root, int r_l, int r_r) {
    int this_r = find_first_unused_r();
    use_reg_r(this_r);

    if (root->lhs->kind == IDENTIFIER) r_l = identifier_reg_index[root->lhs->val - 'x'];
    if (root->rhs->kind == IDENTIFIER) r_r = identifier_reg_index[root->rhs->val - 'x'];

    if (r_l != -1) {    // register
        printf("%s r%d r%d", op_name, this_r, r_l);
    } else if (root->lhs->kind == CONSTANT) {    // constant
        printf("%s r%d %d", op_name, this_r, root->lhs->val);
    } else {
        printf("WARNING %s\n", op_name);
    }

    if (r_r != -1) {    // register
        printf(" r%d\n", r_r);
    } else if (root->rhs->kind == CONSTANT) {    // constant
        printf(" %d\n", root->rhs->val);
    }

    free_reg(r_l, -1, r_r);
    return this_r;
}
int pre_op(AST* root, Kind kind) {
    load_identifier(root->mid->val - 'x');
    int identifier_r = identifier_reg_index[root->mid->val - 'x'];
    if (kind == PREINC) printf("add r%d r%d 1\n", identifier_r, identifier_r);
    else if (kind == PREDEC) printf("sub r%d r%d 1\n", identifier_r, identifier_r);

    printf("store [%d] r%d\n", (root->mid->val - 'x') * 4, identifier_r);

    return identifier_r;
}
int codegen(AST* root, Kind parent_kind) {    // -> return @register_number if exists else return -1
    const static char* KindName[20] = {"assign", "add", "sub", "mul", "div", "rem", "inc", "dec", "inc", "dec", "identifier", "constant", "lpar", "rpar", "plus", "minus", "end"};
    // TODO: Implement your codegen in your own way.
    // You may modify the function parameter or the return type, even the whole structure as you wish.
    if (root == NULL) return -1;
    // printf("thiskind=%s, parent=%s\n", KindName[(root->kind != -1) ? root->kind : 16], KindName[(parent_kind != -1) ? parent_kind : 16]);
    if (root->lhs == NULL && root->mid == NULL && root->lhs == NULL) {
        if (parent_kind != ASSIGN && root->kind == IDENTIFIER && identifier_reg_index[root->val - 'x'] == -1) {    // need to load
            return load_identifier(root->val - 'x');
        } else return -1;
    }
    int r_l = codegen(root->lhs, root->kind);
    int r_m = codegen(root->mid, root->kind);
    int r_r = codegen(root->rhs, root->kind);

    // printf(", r_l=%d, r_m=%d, r_r=%d\n", r_l, r_m, r_r);
    // ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END
    int res;
    switch (root->kind) {
        case ASSIGN:
            if (root->rhs->kind == CONSTANT) {
                int this_r = load_identifier(root->lhs->val - 'x');
                if (root->rhs->val < 0) {
                    printf("sub r%d 0 %d\n", this_r, abs(root->rhs->val));
                } else {
                    printf("add r%d 0 %d\n", this_r, root->rhs->val);
                }
                printf("store [%d] r%d\n", (root->lhs->val - 'x') * 4, this_r);
            } else {
                printf("store [%d] r%d\n", (root->lhs->val - 'x') * 4, r_r);
            }

            if (parent_kind != END) {    // not the outermost ASSIGN
                load_identifier(root->lhs->val - 'x');
                res = identifier_reg_index[root->lhs->val - 'x'];
            } else {    // the outermost ASSIGN
                res = -1;
            }
            break;
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
            res = basic_op(KindName[root->kind], root, r_l, r_r);
            break;
        case PREINC:
        case PREDEC:
            res = pre_op(root, root->kind);
            break;
        case POSTINC:
            identifier_post_delta[root->mid->val - 'x']++;
            res = r_m;
            break;
        case POSTDEC:
            identifier_post_delta[root->mid->val - 'x']--;
            res = r_m;
            break;
        case LPAR:
        case RPAR:
            res = r_m;
            break;
        default:
            res = r_m;
            break;
    }

    // printf("parent_kind=%s\n", KindName[parent_kind]);
    if (parent_kind == END) {
        for (int i = 0; i < 3; i++) {
            if (identifier_post_delta[i] != 0) {
                load_identifier(i);
                printf("%s r%d r%d %d\n", ((identifier_post_delta[i] > 0) ? "add" : "sub"), identifier_reg_index[i], identifier_reg_index[i], abs(identifier_post_delta[i]));
                printf("store [%d] r%d\n", i * 4, identifier_reg_index[i]);
            }
        }
    }

    return res;
}

void freeAST(AST* now) {
    if (now == NULL) return;
    freeAST(now->lhs);
    freeAST(now->mid);
    freeAST(now->rhs);
    free(now);
}

void token_print(Token* in, size_t len) {
    const static char KindName[][20] = {"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus", "End"};
    const static char KindSymbol[][20] = {"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"};
    const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
    const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
    for (int i = 0; i < len; i++) {
        switch (in[i].kind) {
            case LPAR:
            case RPAR:
            case PREINC:
            case PREDEC:
            case ADD:
            case SUB:
            case MUL:
            case DIV:
            case REM:
            case ASSIGN:
            case PLUS:
            case MINUS:
                printf(format_str, i, KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
                break;
            case CONSTANT:
                printf(format_int, i, KindName[in[i].kind], "value", in[i].val);
                break;
            case IDENTIFIER:
                printf(format_str, i, KindName[in[i].kind], "name", (char*)(&(in[i].val)));
                break;
            case END:
                printf("<Index = %3d>: %-10s\n", i, KindName[in[i].kind]);
                break;
            default:
                puts("=== unknown token ===");
        }
    }
}

void AST_print(AST* head) {
    static char indent_str[MAX_LENGTH] = "  ";
    static int indent = 2;
    const static char KindName[][20] = {"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"};
    const static char format[] = "%s\n";
    const static char format_str[] = "%s, <%s = %s>\n";
    const static char format_val[] = "%s, <%s = %d>\n";
    if (head == NULL) return;
    char* indent_now = indent_str + indent;
    indent_str[indent - 1] = '-';
    printf("%s", indent_str);
    indent_str[indent - 1] = ' ';
    if (indent_str[indent - 2] == '`') indent_str[indent - 2] = ' ';
    switch (head->kind) {
        case ASSIGN:
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
        case PREINC:
        case PREDEC:
        case POSTINC:
        case POSTDEC:
        case LPAR:
        case RPAR:
        case PLUS:
        case MINUS:
            printf(format, KindName[head->kind]);
            break;
        case IDENTIFIER:
            printf(format_str, KindName[head->kind], "name", (char*)&(head->val));
            break;
        case CONSTANT:
            printf(format_val, KindName[head->kind], "value", head->val);
            break;
        default:
            puts("=== unknown AST type ===");
    }
    indent += 2;
    strcpy(indent_now, "| ");
    AST_print(head->lhs);
    strcpy(indent_now, "` ");
    AST_print(head->mid);
    AST_print(head->rhs);
    indent -= 2;
    (*indent_now) = '\0';
}
