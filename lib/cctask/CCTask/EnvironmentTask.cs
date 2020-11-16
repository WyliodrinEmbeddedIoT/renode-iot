using System;
using Microsoft.Build.Utilities;
using Microsoft.Build.Framework;

namespace CCTask
{
	public class EnvironmentTask : Task
	{
		[Output]
		public int OsPointerSize { get; private set; }

		[Output]
		public int FrameworkPointerSize { get; private set; }

		public override bool Execute()
		{
			FrameworkPointerSize = IntPtr.Size * 8;
			OsPointerSize = Environment.Is64BitOperatingSystem ? 64 : 32;
			return true;
		}

	}
}

