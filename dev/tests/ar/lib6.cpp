namespace MyNamespace {
namespace SubNamespace {
struct MyStruct {
  MyStruct() = default;
  MyStruct(int v1) : value1(v1), value2(v1) {}

  void l6_member_function() {
    value1 = 42;
    value2 = 24;
  }

  int value1;
  int value2;
};
} // namespace SubNamespace
} // namespace MyNamespace

int l6() {
  MyNamespace::SubNamespace::MyStruct myObject;
  myObject.value1 = 42;
  myObject.value2 = 24;

  return 0;
}

thread_local int tls_var = 0;