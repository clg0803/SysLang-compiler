import os
import subprocess
import concurrent.futures
from multiprocessing import Manager

# ANSI颜色转义序列
GREEN = "\033[92m"
RED = "\033[91m"
ENDC = "\033[0m"

# 获取测试样例文件列表
test_dir = './test/functional/'
test_files = [name for name in os.listdir(test_dir) if name.endswith('.sy')]
test_files.sort()
all_files = set([name for name in os.listdir(test_dir)])

# 全局变量
passed = 0
failed_programs = Manager().list()

# 定义单个测试样例的执行函数
def run_test(test_file):
    test_case = os.path.splitext(test_file)[0]
    print(f"运行测试用例: {test_case}")

    # 构建文件路径
    # 不一定有input，一定有output
    input_file = os.path.join(test_dir, f"{test_case}.in")
    output_file = os.path.join(test_dir, f"{test_case}.out")

    # 编译源代码文件
    compile_cmd = f"./build/crash {test_dir}{test_file} -S -o {test_case}.s 2>/dev/null && gcc -march=armv7-a -mfpu=vfp {test_case}.s ./test/libsysy.a -static -o {test_case}"
    print(compile_cmd)
    compile_process = subprocess.run(compile_cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if compile_process.returncode != 0:
        # 编译失败
        print(f"{RED}失败{ENDC}: 测试用例编译失败: {test_case}")
        print(f"错误信息:\n{compile_process.stderr.decode()}")
        failed_programs.append(test_case)
        return False

    try:
        # 运行测试样例并与预期输出进行对比
        run_cmd = f"./{test_case} < {input_file}"
        if f"{test_case}.in" not in all_files:
            run_cmd = f"./{test_case}"
        run_process = subprocess.run(run_cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=10)
        output = open(output_file, 'r').read().split('\n')
        returncode = output[-2]  # 最后有个空行,去掉最后的空行
        expected_output = output[:-2]
        if run_process.returncode != int(returncode):
            # 运行错误
            print(f"{RED}失败{ENDC}: 测试用例运行失败: {test_case}")
            print("返回值不匹配")
            failed_programs.append(test_case)
            return False

        actual_output = run_process.stdout.decode().split('\n')
        flag = True
        # 时间输出是stderr
        for i in range(len(expected_output)):
            if expected_output[i] != actual_output[i]:
                flag = False
                # 输出不匹配
                print(f"{RED}失败{ENDC}: 测试用例运行失败: {test_case}")
                print("预期输出:")
                print(expected_output[i])
                print("实际输出:")
                print(actual_output[i])
                failed_programs.append(test_case)
                return False

        # 测试通过
        if flag:
            print(f"{GREEN}通过{ENDC}: 测试用例通过: {test_case}")
            return True

    except subprocess.TimeoutExpired:
        # 超时
        print(f"{RED}失败{ENDC}: 测试用例超时: {test_case}")
        failed_programs.append(test_case)
        return False


# 使用多进程池执行测试样例
with concurrent.futures.ProcessPoolExecutor() as executor:
    # 将测试样例提交给进程池，并获取结果
    results = executor.map(run_test, test_files)

    # 统计测试结果
    for result in results:
        if result:
            passed += 1

# 删除产生的目标文件
for test_file in test_files:
    test_case = os.path.splitext(test_file)[0]
    if os.path.exists(test_case):
        os.remove(test_case)
    if os.path.exists(f"{test_case}.s"):
        os.remove(f"{test_case}.s")
    if os.path.exists(f"{test_case}.o"):
        os.remove(f"{test_case}.o")

# 输出失败的程序名称
print("失败的程序:")
for program in failed_programs:
    print(program)

# 统计失败的测试用例数量
failed = len(failed_programs)

print(f"\n总共通过: {passed}\n总共失败: {failed}\n")
