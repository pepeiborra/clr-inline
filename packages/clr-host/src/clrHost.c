
void * global_getPointerToMethod;

void getPointerToMethod_set(void * getPointerToMethod)
{
  global_getPointerToMethod = getPointerToMethod;
}

void * getPointerToMethod_get()
{
  return global_getPointerToMethod;
}

