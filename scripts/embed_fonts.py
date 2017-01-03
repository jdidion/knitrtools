#!/usr/bin/python
import sys
from Quartz.PDFKit import PDFDocument
from Foundation import NSURL
for f in [ a.decode('utf-8') for a in sys.argv[1:] ]:
    PDFDocument.alloc().initWithURL_(NSURL.fileURLWithPath_(f))
