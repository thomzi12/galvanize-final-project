import requests
import zipfile
import StringIO
import json
import os
import pandas as pd
import threading
# from timeit import Timer

class pair_scraper(object):
    '''
    This class provides functionality to scrape desired students pair
    programming code and find its length.
    '''

    def __init__(self, username_list, repo_list, extension_list=['.py',
            '.ipynb']):
        self.auth = ('thomzi12', os.environ['GITHUB_PASSWORD'])
        self.extensions = extension_list
        self.username_list = username_list
        self.repo_list = repo_list
        self.results = []

    def concurrent_threading(self):
        #q = Queue.Queue()
        threads = []
        for username in self.username_list:
            for repo in self.repo_list:
                repo_url = 'https://github.com/{}/{}/zipball/master'.format(
                    username, repo)
                t = threading.Thread(target=self.length_scraper3,
                    args=(username, repo, repo_url))
                t.daemon = True
                t.start()
                threads.append(t)
        for thread in threads:
            thread.join()
        self.to_csv()

    def to_csv(self):
        df_columns = ['gh_username', 'repo_name', 'filename', 'file_length']
        pd.DataFrame(self.results, columns=df_columns).to_csv(
            'pair_programming_data1.csv')
        print 'csv printed'

    def length_scraper3(self, username, repo, repo_url):
        '''
        INPUT: Link to a repo, a file extension
        OUTPUT: List of file data, i.e. [repo, username, filename, length]
        '''
        zf = self.request_zipfile(repo_url)
        repo_data = []
        if zf:
            for filename in zf.namelist():
                if self.pair_check(filename):
                    repo_data.append([username, repo, filename.split('/')[-1], self.get_length(zf, filename)])
        if repo_data:
            self.results += repo_data


    def pair_check(self, filename):
        '''
        Check if filename is candidate for pair programming code
        '''
        for extension in self.extensions:
            if filename.endswith(extension):
                return True
        return False


    def request_zipfile(self, repo_url):
        '''
        INPUT: Repository URL
        OUTPUT: zipfile object or None if request fails
        '''
        print repo_url
        r = requests.get(repo_url, auth=self.auth, timeout = 10)
        if r.status_code != 200:
            print "Getting {} failed: {}".format('/'.join(repo_url.split('/')[3:5]), r.status_code)
            return None
        else:
            return zipfile.ZipFile(StringIO.StringIO(r.content))

    def get_length(self, zf, filename):
        '''
        INPUT: zipfile object, filename
        OUTPUT: number of characters in file (int)
        '''

        length = 0
        if filename.endswith(".ipynb"):
            f = zf.open(filename, 'r')  # input.ipynb
            try:
                j = json.load(f)
                if j["nbformat"] >= 4:
                    for cell in j["cells"]:
                        for line in cell["source"]:
                            if line[0] != "#":
                                length += (len(line) - line.count(' '))
                else:
                    for cell in j["worksheets"][0]["cells"]:
                        if "input" in cell.keys():
                            for line in cell["input"]:
                                if line[0] != "#":
                                    length += (len(line) - line.count(' '))
            except ValueError:
                return 'Error'
        else:
            with zf.open(filename) as f:
                for line in f:
                    if line[0] != "#":
                        length += (len(line) - line.count(' '))
        return length


def get_students():
    df = pd.read_excel('galvanize_data.xlsx')
    series = df.github.apply(str)
    username_list = series.tolist()
    return username_list

if __name__ == "__main__":

    students = get_students()
    repo = ['probability', 'logistic-regression',
            'gradient-descent', 'svm', 'web-scraping', 'time-series',
            'graphs', 'high-performance-python', 'spark']

    scraper = pair_scraper(students, repo)
    scraper.concurrent_threading()

    # Code snippet to time code for efficiency
    # t = Timer(lambda: scraper.concurrent_threading())
    # print "Completed using threads in %s seconds." % t.timeit(1)
