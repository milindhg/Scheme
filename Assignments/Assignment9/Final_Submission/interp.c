#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "interp.h"

void *ktr_empty(void *dismount) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_kt;
  _data->u._empty._dismount = dismount;
  return (void *)_data;
}

void *ktr_innerr_if(void *conseq, void *alt, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_if_kt;
  _data->u._innerr_if._conseq = conseq;
  _data->u._innerr_if._alt = alt;
  _data->u._innerr_if._envr__ex__ = envr__ex__;
  _data->u._innerr_if._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_innerr_mult(void *x, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_mult_kt;
  _data->u._innerr_mult._x = x;
  _data->u._innerr_mult._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_outerr_mult(void *randr2, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr_mult_kt;
  _data->u._outerr_mult._randr2 = randr2;
  _data->u._outerr_mult._envr__ex__ = envr__ex__;
  _data->u._outerr_mult._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_innerr_subr1(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_subr1_kt;
  _data->u._innerr_subr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_innerr_zero(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_zero_kt;
  _data->u._innerr_zero._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_innerr_return(void *vexp, void *envr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_return_kt;
  _data->u._innerr_return._vexp = vexp;
  _data->u._innerr_return._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *ktr_innerr_let(void *body, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_let_kt;
  _data->u._innerr_let._body = body;
  _data->u._innerr_let._envr__ex__ = envr__ex__;
  _data->u._innerr_let._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_innerr_ratorr_rand(void *y, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _innerr_ratorr_rand_kt;
  _data->u._innerr_ratorr_rand._y = y;
  _data->u._innerr_ratorr_rand._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_outerr_ratorr_rand(void *rand, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _outerr_ratorr_rand_kt;
  _data->u._outerr_ratorr_rand._rand = rand;
  _data->u._outerr_ratorr_rand._envr__ex__ = envr__ex__;
  _data->u._outerr_ratorr_rand._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *closr_closure(void *code, void *envr__ex__r__ex__) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._code = code;
  _data->u._closure._envr__ex__r__ex__ = envr__ex__r__ex__;
  return (void *)_data;
}

void *envrr_empty() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_envr;
  return (void *)_data;
}

void *envrr_extend(void *arg, void *envr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extend_envr;
  _data->u._extend._arg = arg;
  _data->u._extend._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *expr_const(void *n) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_exp;
  _data->u._const._n = n;
  return (void *)_data;
}

void *expr_var(void *v) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_exp;
  _data->u._var._v = v;
  return (void *)_data;
}

void *expr_if(void *test, void *conseq, void *alt) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_exp;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *expr_mult(void *randr1, void *randr2) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_exp;
  _data->u._mult._randr1 = randr1;
  _data->u._mult._randr2 = randr2;
  return (void *)_data;
}

void *expr_subr1(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_exp;
  _data->u._subr1._rand = rand;
  return (void *)_data;
}

void *expr_zero(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_exp;
  _data->u._zero._rand = rand;
  return (void *)_data;
}

void *expr_capture(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _capture_exp;
  _data->u._capture._body = body;
  return (void *)_data;
}

void *expr_return(void *vexp, void *kexp) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _return_exp;
  _data->u._return._vexp = vexp;
  _data->u._return._kexp = kexp;
  return (void *)_data;
}

void *expr_let(void *vexp, void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_exp;
  _data->u._let._vexp = vexp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *expr_lambda(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_exp;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *expr_app(void *rator, void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_exp;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
expr = (void *)expr_app(expr_app(expr_lambda(expr_lambda(expr_var((void *)1))),expr_const((void *)5)),expr_const((void *)6));
env = (void *)envrr_empty();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Test 1 output: %d\n", (int)v);expr = (void *)expr_app(expr_lambda(expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5))),expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))));
env = (void *)envrr_empty();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Test 2 output: %d\n", (int)v);expr = (void *)expr_mult(expr_const((void *)2),expr_capture(expr_mult(expr_const((void *)5),expr_return(expr_mult(expr_const((void *)2),expr_const((void *)6)),expr_var((void *)0)))));
env = (void *)envrr_empty();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Test 3 output: %d\n", (int)v);expr = (void *)expr_let(expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))),expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5)));
env = (void *)envrr_empty();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Test 4 output: %d\n", (int)v);}

void applyr__m__k()
{
kt* _c = (kt*)k;
switch (_c->tag) {
case _empty_kt: {
void *dismount = _c->u._empty._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _innerr_if_kt: {
void *conseq = _c->u._innerr_if._conseq;
void *alt = _c->u._innerr_if._alt;
void *envr__ex__ = _c->u._innerr_if._envr__ex__;
void *kr__ex__ = _c->u._innerr_if._kr__ex__;
if(v) {
  k = (void *)kr__ex__;
expr = (void *)conseq;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;

} else {
  k = (void *)kr__ex__;
expr = (void *)alt;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;

}
break; }
case _innerr_mult_kt: {
void *x = _c->u._innerr_mult._x;
void *kr__ex__ = _c->u._innerr_mult._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(void *)((int)x * (int)v);
pc = &applyr__m__k;
break; }
case _outerr_mult_kt: {
void *randr2 = _c->u._outerr_mult._randr2;
void *envr__ex__ = _c->u._outerr_mult._envr__ex__;
void *kr__ex__ = _c->u._outerr_mult._kr__ex__;
k = (void *)ktr_innerr_mult(v,kr__ex__);
expr = (void *)randr2;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _innerr_subr1_kt: {
void *kr__ex__ = _c->u._innerr_subr1._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(void *)((int)v - (int)(void *)1);
pc = &applyr__m__k;
break; }
case _innerr_zero_kt: {
void *kr__ex__ = _c->u._innerr_zero._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(v == 0);
pc = &applyr__m__k;
break; }
case _innerr_return_kt: {
void *vexp = _c->u._innerr_return._vexp;
void *envr__ex__ = _c->u._innerr_return._envr__ex__;
k = (void *)v;
expr = (void *)vexp;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _innerr_let_kt: {
void *body = _c->u._innerr_let._body;
void *envr__ex__ = _c->u._innerr_let._envr__ex__;
void *kr__ex__ = _c->u._innerr_let._kr__ex__;
k = (void *)kr__ex__;
expr = (void *)body;
env = (void *)envrr_extend(v,envr__ex__);
pc = &valuer__m__ofr__m__cps;
break; }
case _innerr_ratorr_rand_kt: {
void *y = _c->u._innerr_ratorr_rand._y;
void *kr__ex__ = _c->u._innerr_ratorr_rand._kr__ex__;
k = (void *)kr__ex__;
c = (void *)y;
a = (void *)v;
pc = &applyr__m__closure;
break; }
case _outerr_ratorr_rand_kt: {
void *rand = _c->u._outerr_ratorr_rand._rand;
void *envr__ex__ = _c->u._outerr_ratorr_rand._envr__ex__;
void *kr__ex__ = _c->u._outerr_ratorr_rand._kr__ex__;
k = (void *)ktr_innerr_ratorr_rand(v,kr__ex__);
expr = (void *)rand;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__closure()
{
clos* _c = (clos*)c;
switch (_c->tag) {
case _closure_clos: {
void *code = _c->u._closure._code;
void *envr__ex__r__ex__ = _c->u._closure._envr__ex__r__ex__;
env = (void *)envrr_extend(a,envr__ex__r__ex__);
expr = (void *)code;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)env;
switch (_c->tag) {
case _empty_envr: {
fprintf(stderr, "unbound variable");
 exit(1);
break; }
case _extend_envr: {
void *arg = _c->u._extend._arg;
void *envr__ex__ = _c->u._extend._envr__ex__;
if((num == 0)) {
  v = (void *)arg;
pc = &applyr__m__k;

} else {
  env = (void *)envr__ex__;
num = (void *)(void *)((int)num - 1);
pc = &applyr__m__env;

}
break; }
}
}

void valuer__m__ofr__m__cps()
{
exp* _c = (exp*)expr;
switch (_c->tag) {
case _const_exp: {
void *n = _c->u._const._n;
v = (void *)n;
pc = &applyr__m__k;
break; }
case _var_exp: {
void *v = _c->u._var._v;
num = (void *)v;
pc = &applyr__m__env;
break; }
case _if_exp: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
k = (void *)ktr_innerr_if(conseq,alt,env,k);
expr = (void *)test;
pc = &valuer__m__ofr__m__cps;
break; }
case _mult_exp: {
void *randr1 = _c->u._mult._randr1;
void *randr2 = _c->u._mult._randr2;
k = (void *)ktr_outerr_mult(randr2,env,k);
expr = (void *)randr1;
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1_exp: {
void *rand = _c->u._subr1._rand;
k = (void *)ktr_innerr_subr1(k);
expr = (void *)rand;
pc = &valuer__m__ofr__m__cps;
break; }
case _zero_exp: {
void *rand = _c->u._zero._rand;
k = (void *)ktr_innerr_zero(k);
expr = (void *)rand;
pc = &valuer__m__ofr__m__cps;
break; }
case _capture_exp: {
void *body = _c->u._capture._body;
expr = (void *)body;
env = (void *)envrr_extend(k,env);
pc = &valuer__m__ofr__m__cps;
break; }
case _return_exp: {
void *vexp = _c->u._return._vexp;
void *kexp = _c->u._return._kexp;
k = (void *)ktr_innerr_return(vexp,env);
expr = (void *)kexp;
pc = &valuer__m__ofr__m__cps;
break; }
case _let_exp: {
void *vexp = _c->u._let._vexp;
void *body = _c->u._let._body;
k = (void *)ktr_innerr_let(body,env,k);
expr = (void *)vexp;
pc = &valuer__m__ofr__m__cps;
break; }
case _lambda_exp: {
void *body = _c->u._lambda._body;
v = (void *)closr_closure(body,env);
pc = &applyr__m__k;
break; }
case _app_exp: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
k = (void *)ktr_outerr_ratorr_rand(rand,env,k);
expr = (void *)rator;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
k= (void *)ktr_empty(dismount);
for(;;) {
pc();
}
}
return 0;
}
