test('conc046', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit GHCi for these two, since they appear to deadlock (23/11/2004 --SDM)
test('conc047', compose(only_compiler_types(['ghc']), 
                        omit_ways(['ghci'])), compile_and_run, [''])
test('conc048', compose(only_compiler_types(['ghc']), 
                        omit_ways(['ghci'])), compile_and_run, [''])

test('conc049', only_compiler_types(['ghc']), compile_and_run, ['-package stm'])
test('conc050', compose(only_compiler_types(['ghc']), extra_run_opts('10000')), compile_and_run, ['-package stm'])

test('conc052', normal, compile_and_run, ['-package stm'])

test('conc053', compose(only_ways(['threaded1','threaded2']),
			skip_if_platform('i386-unknown-mingw32')),
		 compile_and_run, ['-package stm'])
test('conc054', normal, compile_and_run, ['-package stm'])
test('conc055', exit_code(1), compile_and_run, ['-package stm'])
test('conc056', only_ways(['threaded1','threaded2']),
                compile_and_run, ['-package stm -package network'])

test('conc060', normal, compile_and_run, ['-package stm'])
test('conc061', normal, compile_and_run, ['-package stm'])
test('conc062', normal, compile_and_run, ['-package stm'])
test('conc063', normal, compile_and_run, ['-package stm'])
