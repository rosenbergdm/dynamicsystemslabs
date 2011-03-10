class MdivProbe(object):
    def __init__(self, seqid, pos, ptype, name):
        self.id = seqid
        self.ptype = ptype
        self.start = pos - 12
        self.stop = pos + 12
        self.name = name
        self.orientation = +1

infile = open('chrProbes.txt', 'rb')

probe_dict = {}

for row in infile.readlines():
    f = row.split()
    p = MdivProbe(f[1], int(f[2]), int(f[4]), f[0])
    probe_dict[f[0]] = p


annodb = annotation.AnnotationDB(probe_dict, dna_db)

annodb.__doc__ = 'MDIV probe annotations'
worldbase.Bio.MDIV.probes = annodb
worldbase.commit()

mdivProbeIntervals = cnestedlist.NLMSA()