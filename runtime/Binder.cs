using System;
using System.Reflection;

namespace Magic
{
    public static class Binder
    {
        // might end up using a custom binder down the line, C#'s default
        // binder is OK for now
        static System.Reflection.Binder _binder = Type.DefaultBinder;
        public static void SelectMethod()
        {
            // _binder.BindToMethod(BindingFlags.Public)
        }
    }
}