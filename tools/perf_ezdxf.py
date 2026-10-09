# usage: perf_ezdxf.py FILE -- ezdxf read and write time (seconds); peak memory is taken by the caller (/usr/bin/time -l)
import sys, time, io
import ezdxf
t = time.perf_counter(); doc = ezdxf.readfile(sys.argv[1]); t1 = time.perf_counter()
out = io.StringIO(); doc.write(out); t2 = time.perf_counter()
print("read %.3f write %.3f" % (t1 - t, t2 - t1))
