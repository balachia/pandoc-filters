# post-process tex files, looking for 
# "%% @..." directives

import sys
import codecs
import re

debug = 0
verbose=True

# define processors
class PostProcessor:
    def process_line(self, line):
        return (line, False)

class ReplaceNextEnvironment(PostProcessor):
    def __init__(self, args):
        self.opened1 = False
        self.openenv = 0
        self.target = args[0]
        self.replacement = args[1]
        
        self.targeto = re.compile(r'\\begin{' + args[0] + '}', re.U)
        self.targetc = re.compile(r'\\end{' + args[0] + '}', re.U)

    def process_line(self, line):
        reso = self.targeto.match(line)
        resc = self.targetc.match(line)

        #print("RNE :: " + line + str(reso) + str(resc) +
                #str(self.targeto.pattern))

        res = (line, False)

        if reso:
            if not self.opened1:
                self.opened1 = True
                line = self.targeto.sub(r'\\begin{' + self.replacement +
                        '}',line)
                res = (line, False)
            self.openenv += 1
        if resc:
            if self.opened1:
                self.openenv -= 1
            if self.openenv == 0:
                line = self.targetc.sub(r'\\end{' + self.replacement +
                        '}',line)
                res = (line, True)

        return res

# set up processor dict
processor_dict = dict()
processor_dict['replace-next-environment'] = ReplaceNextEnvironment

ppdirective_re = re.compile(r'^%% @(\S+) (.*)')

def main():
    # announce
    if verbose:
        print('Python Pandoc Postprocessor')

    # read args
    args = sys.argv
    if len(args) == 1:
        raise SystemExit('No arguments supplied')
    else:
        infile = args[1]

    # announce
    if verbose:
        print('\tProcessing: %s' % infile)

    # read in file, lazy as hell
    with codecs.open(infile, mode='r', encoding='utf8') as fin:
        lines = [line.strip() for line in fin]

    if debug > 0:
        print(lines)

    processors = list()
    outlines = list()
    for line in lines:
        res = ppdirective_re.match(line)

        if debug > 0:
            print(line)

        # check for new processors
        if res:
            directive = res.groups()[0]
            dir_args = [x.strip() for x in (res.groups()[1]).split()]
            processors.append(processor_dict[directive](dir_args))

            if debug > 0:
                print('\tDIRECTIVE: %s, ARGS: %s' % (directive, dir_args))
            continue
        elif debug > 1:
            print('\t(NO DIRECTIVE)')

        # run the processors
        drop_processor = list()
        for processor in processors:
            res = processor.process_line(line)
            line = res[0]
            drop_processor.append(res[1])
        if debug > 1:
            print(" ==> " + str(line))
        outlines.append(line)

        if debug > 1:
            print(processors)

        # drop any finished processors
        processors = [processors[i] for i in range(len(processors)) if not
                drop_processor[i]] 

        if debug > 1:
            print(processors)

    # write everything out
    if debug > 0:
        print(outlines)

    with codecs.open(infile, mode='w', encoding='utf8') as fout:
        for line in outlines:
            fout.write(line + "\n")

    if verbose:
        print('\tPPP done!')


if __name__ == "__main__":
    main()
