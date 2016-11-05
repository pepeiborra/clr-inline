
void * global_ICorRuntimeHost;
void * global_IClrRuntimeHost;

void setHostRefs(void * ICorRuntimeHost, void * IClrRuntimeHost)
{
  global_ICorRuntimeHost = ICorRuntimeHost;
  global_IClrRuntimeHost = IClrRuntimeHost;
}

void * getICorRuntimeHost()
{
  return global_ICorRuntimeHost;
}

void * getIClrRuntimeHost()
{
  return global_IClrRuntimeHost;
}

