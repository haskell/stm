test('stm046', [], compile_and_run, [''])
test('stm047', [], compile_and_run, [''])
test('stm048', [], compile_and_run, [''])

test('stm050', [extra_run_opts('10000')], compile_and_run, ['-package stm'])

test('stm054', normal, compile_and_run, ['-package stm'])
test('stm055', exit_code(1), compile_and_run, ['-package stm'])
test('stm056', only_ways(['threaded1','threaded2']),
                compile_and_run, ['-package stm'])

test('stm061', normal, compile_and_run, ['-package stm'])
test('T2411', ignore_stdout, compile_and_run, ['-package stm'])
test('stm064', normal, compile_and_run, ['-package stm'])
test('stm065', normal, compile_and_run, ['-package stm'])
test('cloneTChan001', normal, compile_and_run, ['-package stm'])

test('T15136', extra_run_opts('20'), compile_and_run, ['-package stm'])
test('T16707', normal, compile_and_run, [''])
