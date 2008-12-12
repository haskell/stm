test('conc046', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit GHCi for these two, since they appear to deadlock (23/11/2004 --SDM)
test('conc047',
     composes([only_compiler_types(['ghc']), 
               omit_ways(['ghci']),
               if_compiler_lt('ghc', '6.9', omit_ways(['ghci', 'threaded2']))]),
     compile_and_run, [''])
test('conc048',
     composes([only_compiler_types(['ghc']), 
               omit_ways(['ghci']),
               if_compiler_lt('ghc', '6.9', omit_ways(['ghci', 'threaded2']))]),
     compile_and_run, [''])

test('conc049', only_compiler_types(['ghc']), compile_and_run, ['-package stm'])
test('conc050', compose(only_compiler_types(['ghc']), extra_run_opts('10000')), compile_and_run, ['-package stm'])

# Was failing prof ways due to #1547, but now apparently succeeds:
test('conc052', if_compiler_lt('ghc', '6.9', 
                               expect_broken_for(1547,['profc','profasm'])),
     compile_and_run, ['-package stm'])

test('conc053', compose(only_ways(['threaded1','threaded2']),
			if_platform('i386-unknown-mingw32', skip)),
		 compile_and_run, ['-package stm'])
test('conc054', normal, compile_and_run, ['-package stm'])
test('conc055', exit_code(1), compile_and_run, ['-package stm'])
test('conc056', only_ways(['threaded1','threaded2']),
                compile_and_run, ['-package stm'])

test('conc060', normal, compile_and_run, ['-package stm'])
test('conc061', normal, compile_and_run, ['-package stm'])
test('conc062', normal, compile_and_run, ['-package stm'])
test('conc063', skip_if_fast, compile_and_run, ['-package stm'])
test('2411', ignore_output, compile_and_run, ['-package stm'])