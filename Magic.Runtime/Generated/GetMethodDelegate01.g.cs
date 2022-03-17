// this file was generated by Magic.Runtime.Callsites -- do not edit it by hand!
using System.Reflection;

namespace Magic
{
    public static partial class DelegateHelpers
    {
        public static CallsiteFunc<object, object> GetMethodDelegate01(MethodBase method)
        {
            CallsiteFunc<object, object> ret;
            if(method.IsStatic)
                ret = (arg0) => 
                {
                    var args = new[] { arg0 };
                    Binder.Shared.ConvertArguments(method, args);
                    return method.Invoke(null, args);
                };
            else if(method.IsConstructor)
                ret = (arg0) => 
                {
                    var ctor = method as ConstructorInfo;
                    var args = new[] { arg0 };
                    Binder.Shared.ConvertArguments(ctor, args);
                    return ctor.Invoke(args);
                };
            else
                ret = (target) => 
                {
                    return method.Invoke(target, null);
                };
            return ret;
        }

    }
}