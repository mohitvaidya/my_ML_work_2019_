import argparse
def samp_udf(path):
    print('it is working, ',path)
if __name__=="__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument('path',help='input_file-path')
    args=parser.parse_args()
    print(args.path)
    samp_udf(args.path)