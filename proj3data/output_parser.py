import re
import pandas as pd
if __name__ == "__main__":
    with open("output2.txt", "r") as output_file:
        output_string = output_file.read()
    average_swap_times = re.findall(r"(?<=Average swap time )[0-9]*.[0-9]*", output_string)
    states = ["Null", "Unsynchronized", "Synchronized", "AcmeSafe"]
    threads = [1, 8, 16, 24, 32, 40]
    sizes = [5, 25, 50, 75, 100, 500]
    thread_results = {state: [] for state in states}
    size_results = {state: [] for state in states}
    i=0
    for state in states:
        for thread in threads:
            thread_results[state].append(average_swap_times[i])
            i+=1
        for size in sizes:
            size_results[state].append(average_swap_times[i])
            i+=1
    thread_df = pd.DataFrame.from_dict(thread_results, orient='index', columns=threads)
    size_df = pd.DataFrame.from_dict(size_results, orient='index', columns=sizes)
    thread_df.to_csv('lnxsrv06_threads.csv', sep = '&', float_format='%.2g')
    size_df.to_csv('lnxsrv06_sizes.csv', sep='&', float_format='%.2g')



        

