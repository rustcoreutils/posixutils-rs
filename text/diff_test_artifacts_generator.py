import subprocess
import os

def generate_test_artifact(command: str , f1_path: str, f2_path: str, destination_path: str):
    with open(destination_path, 'w') as output_file:
        final_command = "{} {} {}".format(command , f1_path , f2_path).split()
        print(final_command)
        subprocess.run(final_command, stdout=output_file)

DEFAULT_COMMAND = "cargo run --bin diff"
ARTIFACTS_DIRECTORY  = os.path.join("tests" , "diff")
F1TXT_PATH = os.path.join(ARTIFACTS_DIRECTORY, "f1.txt")
F1TXT_EOLS_PATH = os.path.join(ARTIFACTS_DIRECTORY, "f1_with_eol_spaces.txt")
F1DIR_PATH = os.path.join(ARTIFACTS_DIRECTORY, "f1")
F2TXT_PATH = os.path.join(ARTIFACTS_DIRECTORY, "f2.txt")
F2DIR_PATH = os.path.join(ARTIFACTS_DIRECTORY, "f2")

generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -C 1") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "context_1_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -C 10") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "context_10_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -c") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "context_output.txt"))
generate_test_artifact(DEFAULT_COMMAND ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "default_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -e") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "edit_script_output.txt"))
generate_test_artifact(DEFAULT_COMMAND ,F1TXT_PATH ,F1TXT_EOLS_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_counting_eol_spacesd_output.txt"))
generate_test_artifact(DEFAULT_COMMAND ,F1DIR_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_f2_default_output.txt"))
generate_test_artifact("{} {}".format( DEFAULT_COMMAND , "-- -r -c") ,F1DIR_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_f2_recursive_context_output.txt"))
generate_test_artifact("{} {}".format( DEFAULT_COMMAND , "-- -r -e") ,F1DIR_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_f2_recursive_ed_output.txt"))
generate_test_artifact("{} {}".format( DEFAULT_COMMAND , "-- -r -f") ,F1DIR_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_f2_recursive_f_output.txt"))
generate_test_artifact("{} {}".format( DEFAULT_COMMAND , "-- -r") ,F1DIR_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_f2_recursive_output.txt"))
generate_test_artifact("{} {}".format( DEFAULT_COMMAND , "-- -r -u") ,F1DIR_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_f2_recursive_unified_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -b") ,F1TXT_PATH ,F1TXT_EOLS_PATH , os.path.join(ARTIFACTS_DIRECTORY , "f1_ignoring_eol_spacesd_output.txt"))
generate_test_artifact(DEFAULT_COMMAND ,F1TXT_PATH ,F2DIR_PATH , os.path.join(ARTIFACTS_DIRECTORY , "file_dir_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -f") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "forward_edit_script_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -u --label F1 --label2 F2") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "label_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -U 0") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "unified_0_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -U 10") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "unified_10_output.txt"))
generate_test_artifact("{} {}".format(DEFAULT_COMMAND , "-- -u") ,F1TXT_PATH ,F2TXT_PATH , os.path.join(ARTIFACTS_DIRECTORY , "unified_output.txt"))
