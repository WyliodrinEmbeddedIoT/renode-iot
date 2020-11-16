using System;
using System.Reflection;

namespace Antmicro.OptionsParser
{
    public class ApplicationInfo
    {
        public void GetInfo(Type t)
        {
            var applicationNameAttribute = t.Assembly.GetCustomAttribute<AssemblyTitleAttribute>();
            if(applicationNameAttribute != null)
            {
                ApplicationName = applicationNameAttribute.Title;
            }

            // assembly version is not available through custom attribute...
            ApplicationVersion = t.Assembly.GetName().Version.ToString();

            var applicationCopyrightAttribute = t.Assembly.GetCustomAttribute<AssemblyCopyrightAttribute>();
            if(applicationCopyrightAttribute != null)
            {
                ApplicationCopyrights = applicationCopyrightAttribute.Copyright;
            }

            ApplicationBinaryName = AppDomain.CurrentDomain.FriendlyName;
        }

        public string ApplicationName { get; private set; }
        public string ApplicationBinaryName { get; private set; }
        public string ApplicationVersion { get; private set; }
        public string ApplicationCopyrights { get; private set; }
    }
}

