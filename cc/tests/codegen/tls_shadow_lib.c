_Thread_local int x = 5;
int get_x(void) { return x; }
int set_shadowed(int x, int y)
{
    int *p = &x;
    {
        extern _Thread_local int x;
        x = *p + y;
    }
    return *p + y;
}
int get_shadowed(int x, int y)
{
    int *p = &x;
    *p += y;
    {
        extern _Thread_local int x;
        return x + *p + y;
    }
}
int *addr_shadowed(int x)
{
    int *p = &x;
    *p = 0;
    {
        extern _Thread_local int x;
        return &x;
    }
}
