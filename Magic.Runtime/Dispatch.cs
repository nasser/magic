using System;
using System.Reflection;
using System.Linq;

namespace Magic
{
    public static class Dispatch
    {
        internal static object InvokeUnwrappingExceptions(MethodBase method, object target, object[] args)
        {
            try
            {
                var ctor = method as ConstructorInfo;
                if(ctor != null)
                    return ctor.Invoke(args);
                else
                    return method.Invoke(target, args);
            }
            catch (TargetInvocationException e)
            {
                throw e.InnerException;
            }
        }

        // (set! (.name o) value)
        public static object SetMember(object o, string name, object value)
        {
            var oType = o.GetType();
            var valueType = value.GetType();
            var field = oType.GetField(name);
            if (field != null)
            {
                if (field.FieldType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, field.FieldType);
                field.SetValue(o, value);
                return value;
            }
            var property = oType.GetProperty(name);
            if (property != null)
            {
                if (property.PropertyType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, property.PropertyType);
                property.SetValue(o, value, null);
                return value;
            }
            throw new Exception($"Could not set member `{name}` on target {o.ToString()}, no such member exists.");
        }

        // (.name o)
        public static object InvokeZeroArityMember(object o, string name)
        {
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(name);
            if (field != null)
                return field.GetValue(o);
            var property = oType.GetProperty(name);
            if (property != null)
                return InvokeUnwrappingExceptions(property.GetGetMethod(), o, null);
            var method = oType.GetMethod(name, Type.EmptyTypes);
            if (method != null)
                return InvokeUnwrappingExceptions(method, o, null);
            throw new Exception($"Could not invoke zero arity member `{name}` on target {(o == null ? "null" : o.ToString())}.");
        }

        internal static MethodBase BindToMethod(BindingFlags bindingFlags, Type t, string name, object[] args)
        {
            var methods = t.GetMethods().Where(m => m.Name == name).ToArray();
            Object state;
            MethodBase method;
            method = null;
            if (methods.Length > 0)
                method = Binder.Shared.BindToMethod(bindingFlags, methods, ref args, null, null, null, out state);
            return method;
        }

        internal static MethodBase BindToConstructor(Type t, object[] args)
        {
            var ctors = t.GetConstructors().Where(c => c.GetParameters().Length == args.Length).ToArray();
            Object state;
            MethodBase ctor;
            ctor = null;
            if (ctors.Length > 0)
                ctor = Binder.Shared.BindToMethod(BindingFlags.Public | BindingFlags.Instance, ctors, ref args, null, null, null, out state);
            return ctor;
        }

        public static object InvokeInstanceMethod(object o, string name, object[] args)
        {
            var method = BindToMethod(BindingFlags.Public | BindingFlags.Instance, o == null ? typeof(object) : o.GetType(), name, args);
            if (method != null)
                return InvokeUnwrappingExceptions(method, o, args);
            var argsString = args.Select(a => a.ToString()).ToArray();
            throw new Exception($"Could not invoke instance member method `{name}` on target {o.ToString()} ({o.GetType()}) with argument types { String.Join(", ", argsString) }.");
        }

        public static object InvokeStaticMethod(object t, string name, object[] args)
        {
            var method = BindToMethod(BindingFlags.Public | BindingFlags.Static, (Type)t, name, args);
            if (method != null)
                return InvokeUnwrappingExceptions(method, null, args);
            var argsString = args.Select(a => a.ToString()).ToArray();
            throw new Exception($"Could not invoke static member method `{name}` on target {t} with argument types { String.Join(", ", argsString) }.");
        }

        public static object InvokeConstructor(Type t, object[] args)
        {
            return Activator.CreateInstance(t, args);
        }
    }
}