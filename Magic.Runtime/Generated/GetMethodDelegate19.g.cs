// this file was generated by Magic.Runtime.Callsites -- do not edit it by hand!
using System.Reflection;

namespace Magic
{
    public static partial class DelegateHelpers
    {
        public static CallsiteFunc<object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object> GetMethodDelegate19(MethodBase method)
        {
            CallsiteFunc<object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object> ret;
            if(method.IsStatic)
                ret = (arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) => 
                {
                    var args = new[] { arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18 };
                    Binder.Shared.ConvertArguments(method, args);
                    return method.Invoke(null, args);
                };
            else if(method.IsConstructor)
                ret = (arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) => 
                {
                    var ctor = method as ConstructorInfo;
                    var args = new[] { arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18 };
                    Binder.Shared.ConvertArguments(ctor, args);
                    return ctor.Invoke(args);
                };
            else
                ret = (target, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18) => 
                {
                    var args = new [] { arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18 };
                    Binder.Shared.ConvertArguments(method, args);
                    return method.Invoke(target, args);
                };
            return ret;
        }

    }
}