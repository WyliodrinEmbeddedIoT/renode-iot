using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Security.Cryptography;
using System.Text;

namespace CCTask
{
	public class FileCacheManager : IDisposable
	{
		public FileCacheManager(string directory = null)
		{
			hasherSource = new ThreadLocal<MD5>(() => MD5.Create());
			hashDb = new Dictionary<DictionaryKey, DictionaryContent>();

			hashDbFile = Path.Combine(directory ?? Directory.GetCurrentDirectory(), HashDbFilename);
			Directory.CreateDirectory(Path.GetDirectoryName(hashDbFile));
			Load();
		}

		public void Dispose()
		{
			Save();
		}

		public bool SourceHasChanged(IEnumerable<string> sources, string args)
		{
			var changed = false;
			foreach(var source in sources)
			{
				changed = changed | SourceHasChanged(source, args);
			}
			return changed;
		}

		private string CalculateMD5(string s)
		{
			var md5 = hasherSource.Value;
			return BitConverter.ToString(md5.ComputeHash(Encoding.UTF8.GetBytes(s))).ToLower().Replace("-", "");
		}

		private void Load()
		{
			if(!File.Exists(hashDbFile))
			{
#if DEBUG
				Console.WriteLine("No hashDb found.");
#endif
				return;
			}
			lock(hashDb)
			{
				foreach(var line in File.ReadLines(hashDbFile))
				{
					var fileAndHash = line.Split(new [] { ';' }, StringSplitOptions.RemoveEmptyEntries);
					hashDb.Add(new DictionaryKey(fileAndHash[0], fileAndHash[1]), new DictionaryContent(fileAndHash[2], false));
				}
			}
		}

		private void Save()
		{
			Directory.CreateDirectory(Path.GetDirectoryName(hashDbFile));
			lock(hashDb)
			{
				File.WriteAllLines(hashDbFile, hashDb.Select(x => string.Format("{0};{1};{2}", x.Key.Path, x.Key.Arguments, x.Value.Hash)));
			}
		}

		private bool SourceHasChanged(string sourcePath, string args)
		{
			if(!File.Exists(sourcePath))
			{
				return true;
			}
			string hash;
			using(var stream = File.OpenRead(sourcePath))
			{
				var hasher = hasherSource.Value;
				hash = BitConverter.ToString(hasher.ComputeHash(stream));
			}

			var result = false;
			var hashKey = new DictionaryKey(sourcePath, args);
			var hashEntry = new DictionaryContent();
			lock(hashDb)
			{
				if(!hashDb.TryGetValue(hashKey, out hashEntry))
				{
					result = true;
				}
				else if(hashEntry.Modified)
				{
					return true;
				}
				else
				{
					result = (hashDb[hashKey].Hash != hash);
				}
				if(result)
				{
					hashDb[hashKey] = new DictionaryContent(hash, true);
				}
#if DEBUG
				Console.WriteLine("Check of file {0} resulted in {1}, key {2}, hash {3}, in db {4}", sourcePath, result, hashKey, hash, hashDbFile);
#endif
			}
			return result;
		}

		private readonly Dictionary<DictionaryKey, DictionaryContent> hashDb;
		private readonly string hashDbFile;
		private readonly ThreadLocal<MD5> hasherSource;

		private struct DictionaryKey
		{
			public DictionaryKey(string path, string arguments)
			{
				this.Path = path;
				this.Arguments = arguments;
			}
			public string Path;
			public string Arguments;
		}

		private struct DictionaryContent
		{
			public DictionaryContent(string hash, bool modified)
			{
				this.Hash = hash;
				this.Modified = modified;
			}
			public string Hash;
			public bool Modified;
		}

		private const string HashDbFilename = "hashes";
	}
}

