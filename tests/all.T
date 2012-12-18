test('stm046', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit GHCi for these two, since they appear to deadlock (23/11/2004 --SDM)
test('stm047',
     composes([only_compiler_types(['ghc']), 
               omit_ways(['ghci']),
               if_compiler_lt('ghc', '6.9', omit_ways(['ghci', 'threaded2']))]),
     compile_and_run, [''])
test('stm048',
     composes([only_compiler_types(['ghc']), 
               omit_ways(['ghci']),
               if_compiler_lt('ghc', '6.9', omit_ways(['ghci', 'threaded2']))]),
     compile_and_run, [''])

test('stm049', [ reqlib('random'), only_compiler_types(['ghc']) ],
                compile_and_run, ['-package stm'])
test('stm050', compose(only_compiler_types(['ghc']), extra_run_opts('10000')), compile_and_run, ['-package stm'])

# Was failing prof ways due to #1547, but now apparently succeeds:
test('stm052', [ reqlib('random'),
                 if_compiler_lt('ghc', '6.9',
                                expect_broken_for(1547,['profc','profasm'])) ],
     compile_and_run, ['-package stm'])

test('stm053', [ reqlib('random'),
                 only_ways(['threaded1','threaded2']),
                 if_platform('i386-unknown-mingw32', skip) ],
               compile_and_run, ['-package stm'])
test('stm054', normal, compile_and_run, ['-package stm'])
test('stm055', exit_code(1), compile_and_run, ['-package stm'])
test('stm056', only_ways(['threaded1','threaded2']),
                compile_and_run, ['-package stm'])

test('stm060', normal, compile_and_run, ['-package stm'])
test('stm061', normal, compile_and_run, ['-package stm'])
test('stm062', normal, compile_and_run, ['-package stm'])
test('stm063', skip_if_fast, compile_and_run, ['-package stm'])
test('2411', ignore_output, compile_and_run, ['-package stm'])
test('3049', normal, compile_and_run, ['-package stm'])
test('4057', normal, compile_and_run, ['-package stm'])
test('stm064', normal, compile_and_run, ['-package stm'])
test('stm065', normal, compile_and_run, ['-package stm'])
