
void * global_ICorRuntimeHost;
void * global_ICLRRuntimeHost;

void setHostRefs(void * ICorRuntimeHost, void * ICLRRuntimeHost)
{
  global_ICorRuntimeHost = ICorRuntimeHost;
  global_ICLRRuntimeHost = ICLRRuntimeHost;
}

void * getICorRuntimeHost()
{
  return global_ICorRuntimeHost;
}

void * getICLRRuntimeHost()
{
  return global_ICLRRuntimeHost;
}

