import sys, collections, time
import ezdxf
from ezdxf import recover
for p in sys.argv[1:]:
    t=time.time()
    try:
        doc = ezdxf.readfile(p)
        how='read'
    except Exception as e:
        doc, aud = recover.readfile(p); how='recover:'+type(e).__name__
    c=collections.Counter(e.dxftype() for e in doc.modelspace())
    allc=collections.Counter()
    for b in doc.blocks:
        for e in b: allc[e.dxftype()]+=1
    print(p.split('/')[-1][:30], doc.dxfversion, doc.encoding, how, 'msp', sum(c.values()), 'blk', len(doc.blocks), sum(allc.values()), 'obj', len(list(doc.objects)), '%.1fs'%(time.time()-t))
    print('   ', dict(c.most_common(12)), '| blocks:', dict(allc.most_common(6)))
