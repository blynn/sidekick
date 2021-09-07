#define WASM_IMPORT(m,n) __attribute__((import_module(m))) __attribute__((import_name(n)));
typedef unsigned u;
void reply_data_append(void *, int) WASM_IMPORT("ic0", "msg_reply_data_append");
void reply(void) WASM_IMPORT("ic0", "msg_reply");
u arg_data_size(void) WASM_IMPORT("ic0", "msg_arg_data_size");
void arg_data_copy(void *, int, int) WASM_IMPORT("ic0", "msg_arg_data_copy");
static u arg_len, arg_i; 
int eof() { return arg_i == arg_len; }
int getchar() {
  if (eof()) return 0;
  static unsigned char buf[1];
  arg_data_copy(buf, arg_i++, 1);
  return *buf;
}
void putchar(char n) {
  static char buf[1];
  *buf = n;
  reply_data_append(buf, 1);
}

void pre_run() { arg_len = arg_data_size(), arg_i = 0; }
void post_run() { reply(); }
