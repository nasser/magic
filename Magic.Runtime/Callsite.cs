using System;
using System.Reflection;
using System.Linq;

namespace Magic
{
    // (set! (.name o) value)
    public class CallSiteSetMember
    {
        string MemberName;

        public CallSiteSetMember(string memberName)
        {
            MemberName = memberName;
        }

        public object Invoke(object o, object value)
        {
            var oType = o.GetType();
            var valueType = value.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
            {
                if (field.FieldType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, field.FieldType);
                field.SetValue(o, value);
                return value;
            }
            var property = oType.GetProperty(MemberName);
            if (property != null)
            {
                if (property.PropertyType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, property.PropertyType);
                property.SetValue(o, value, null);
                return value;
            }
            throw new Exception($"Could not set member `{MemberName}` on target {o.ToString()}, no such member exists.");
        }
    }

    // (.name o)
    public class CallSiteZeroArityMember
    {
        string MemberName;

        public CallSiteZeroArityMember(string memberName)
        {
            MemberName = memberName;
        }

        public object Invoke(object o)
        {
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
                return field.GetValue(o);
            var property = oType.GetProperty(MemberName);
            if (property != null)
                return Dispatch.InvokeUnwrappingExceptions(property.GetGetMethod(), o, null);
            var method = oType.GetMethod(MemberName, Type.EmptyTypes);
            if (method != null)
                return Dispatch.InvokeUnwrappingExceptions(method, o, null);
            throw new Exception($"Could not invoke zero arity member `{MemberName}` on target {(o == null ? "null" : o.ToString())}.");
        }

    }

    // TODO artities
    public class CallSiteInstanceMethod
    {
        string MethodName;

        public CallSiteInstanceMethod(string methodName)
        {
            MethodName = methodName;
        }

        public object Invoke(object o, object[] args)
        {
            var method = Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Instance, o.GetType(), MethodName, args);
            if (method != null)
                return Dispatch.InvokeUnwrappingExceptions(method, o, args);
            var argsString = args.Select(a => a.ToString()).ToArray();
            throw new Exception($"Could not invoke instance member method `{MethodName}` on target {o.ToString()} ({o.GetType()}) with argument types { String.Join(", ", argsString) }.");
        }

    }

    // TODO artities
    public class CallSiteStaticMethod
    {
        Type TargetType;
        string MethodName;

        public CallSiteStaticMethod(Type targetType, string methodName)
        {
            TargetType = targetType;
            MethodName = methodName;
        }

        public object Invoke(object[] args)
        {
            var method = Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Static, TargetType, MethodName, args);
            if (method != null)
                return Dispatch.InvokeUnwrappingExceptions(method, null, args);
            var argsString = args.Select(a => a.ToString()).ToArray();
            throw new Exception($"Could not invoke static member method `{MethodName}` on target {TargetType} with argument types { String.Join(", ", argsString) }.");
        }
    }

    // TODO artities
    public class CallSiteConstructor
    {
        Type TargetType;

        public CallSiteConstructor(Type targetType)
        {
            TargetType = targetType;
        }

        public object Invoke(object[] args)
        {
            return Activator.CreateInstance(TargetType, args);
        }
    }
}