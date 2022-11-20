using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ManagedDotnetProfiler
{
    internal class TargetClass
    {
        public static void TargetVoidMethod()
        {
            // Console.WriteLine("strange");
        }

        public static string TargetMethod(string message)
        {
            // Console.WriteLine("strange");
            return $"[TargetMethod]{message}";
        }
    }
}
