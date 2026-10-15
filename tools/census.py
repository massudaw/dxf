"""Entities of the samples by type, by number and by bytes, and bytes by section.

usage: census.py FILE...   (text DXF only; binary files are skipped)
"""
import sys
from collections import Counter

count, size, sect = Counter(), Counter(), Counter()
total = 0
for path in sys.argv[1:]:
    data = open(path, 'rb').read()
    if data.startswith(b'AutoCAD Binary'):
        continue
    total += len(data)
    lines = data.split(b'\n')
    section, ent, ent_bytes = None, None, 0
    i = 0
    n = len(lines)
    while i + 1 < n:
        code = lines[i].strip()
        val = lines[i + 1].strip()
        step = len(lines[i]) + len(lines[i + 1]) + 2
        if code == b'0':
            if ent is not None:
                count[(section, ent)] += 1
                size[(section, ent)] += ent_bytes
            if val == b'SECTION' or val == b'ENDSEC' or val == b'EOF':
                ent = None
                if val == b'ENDSEC':
                    section = None
            else:
                ent, ent_bytes = val.decode('latin1'), 0
        elif code == b'2' and ent is None and section is None:
            section = val.decode('latin1')
        if ent is not None:
            ent_bytes += step
        if section is not None:
            sect[section] += step
        i += 2

print('total bytes', total)
print('%-10s %10s %6s' % ('section', 'bytes', '%'))
for s, b in sect.most_common():
    print('%-10s %10d %5.1f%%' % (s, b, 100.0 * b / total))
print()
print('%-10s %-16s %9s %11s %6s %8s' % ('section', 'type', 'count', 'bytes', '%', 'B/entity'))
for k, b in size.most_common(40):
    print('%-10s %-16s %9d %11d %5.1f%% %8d' % (k[0], k[1], count[k], b, 100.0 * b / total, b // count[k]))
