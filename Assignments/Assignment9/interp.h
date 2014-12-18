void *k, *expr, *v, *c, *a, *env, *num;

void (*pc)();

struct exp;
typedef struct exp exp;
struct exp {
  enum {
    _const_exp,
    _var_exp,
    _if_exp,
    _mult_exp,
    _subr1_exp,
    _zero_exp,
    _capture_exp,
    _return_exp,
    _let_exp,
    _lambda_exp,
    _app_exp
  } tag;
  union {
    struct { void *_n; } _const;
    struct { void *_v; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_randr1; void *_randr2; } _mult;
    struct { void *_rand; } _subr1;
    struct { void *_rand; } _zero;
    struct { void *_body; } _capture;
    struct { void *_vexp; void *_kexp; } _return;
    struct { void *_vexp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *expr_const(void *n);
void *expr_var(void *v);
void *expr_if(void *test, void *conseq, void *alt);
void *expr_mult(void *randr1, void *randr2);
void *expr_subr1(void *rand);
void *expr_zero(void *rand);
void *expr_capture(void *body);
void *expr_return(void *vexp, void *kexp);
void *expr_let(void *vexp, void *body);
void *expr_lambda(void *body);
void *expr_app(void *rator, void *rand);

void valuer__m__ofr__m__cps();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _empty_envr,
    _extend_envr
  } tag;
  union {
    struct { char dummy; } _empty;
    struct { void *_arg; void *_envr__ex__; } _extend;
  } u;
};

void *envrr_empty();
void *envrr_extend(void *arg, void *envr__ex__);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_code; void *_envr__ex__r__ex__; } _closure;
  } u;
};

void *closr_closure(void *code, void *envr__ex__r__ex__);

void applyr__m__closure();
struct cont;
typedef struct cont cont;
struct cont {
  enum {
    _empty_cont,
    _innerr_if_cont,
    _innerr_mult_cont,
    _outerr_mult_cont,
    _innerr_subr1_cont,
    _innerr_zero_cont,
    _innerr_return_cont,
    _innerr_let_cont,
    _innerr_ratorr_rand_cont,
    _outerr_ratorr_rand_cont
  } tag;
  union {
    struct { void *_dismount; } _empty;
    struct { void *_conseq; void *_alt; void *_envr__ex__; void *_kr__ex__; } _innerr_if;
    struct { void *_x; void *_kr__ex__; } _innerr_mult;
    struct { void *_randr2; void *_envr__ex__; void *_kr__ex__; } _outerr_mult;
    struct { void *_kr__ex__; } _innerr_subr1;
    struct { void *_kr__ex__; } _innerr_zero;
    struct { void *_vexp; void *_envr__ex__; } _innerr_return;
    struct { void *_body; void *_envr__ex__; void *_kr__ex__; } _innerr_let;
    struct { void *_y; void *_kr__ex__; } _innerr_ratorr_rand;
    struct { void *_rand; void *_envr__ex__; void *_kr__ex__; } _outerr_ratorr_rand;
  } u;
};

void *contr_empty(void *dismount);
void *contr_innerr_if(void *conseq, void *alt, void *envr__ex__, void *kr__ex__);
void *contr_innerr_mult(void *x, void *kr__ex__);
void *contr_outerr_mult(void *randr2, void *envr__ex__, void *kr__ex__);
void *contr_innerr_subr1(void *kr__ex__);
void *contr_innerr_zero(void *kr__ex__);
void *contr_innerr_return(void *vexp, void *envr__ex__);
void *contr_innerr_let(void *body, void *envr__ex__, void *kr__ex__);
void *contr_innerr_ratorr_rand(void *y, void *kr__ex__);
void *contr_outerr_ratorr_rand(void *rand, void *envr__ex__, void *kr__ex__);

void applyr__m__continuation();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

