# usage: genbin.py IN.dxf OUTDIR NAME -- ezdxf writes IN as OUTDIR/NAME.txt.dxf and OUTDIR/NAME.bin.dxf
import sys, ezdxf
doc = ezdxf.readfile(sys.argv[1])
doc.saveas(f"{sys.argv[2]}/{sys.argv[3]}.txt.dxf")
doc.saveas(f"{sys.argv[2]}/{sys.argv[3]}.bin.dxf", fmt="bin")
