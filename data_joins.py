import pandas as pd
import os
import numpy as np
from fuzzywuzzy import process # Will likely need to pip install if you haven't already
from collections import defaultdict


def grab_pair_assignment_data():
    return pd.read_csv('../repo-name.csv')


def grab_length_data(directory):

    
    '''
    INPUT: Current working directory
    OUTPUT: Dataframe consisting of all csv files in directory, concatenated together
    '''
    # Could be rewritten to be generalizable -- not at the moment
    df = pd.read_csv('pair_programming_data1.csv', index_col=0)
    for filename in os.listdir(directory):
        if filename != 'pair_programming_data1.csv' and filename.endswith('csv'):
            df = pd.concat([df, pd.read_csv(filename, index_col=0)])
    return df


def grab_final_data():
    return pd.read_excel('../../galvanize_data.xlsx')


def _pair_checker(filename):
    '''
    find_relevant_columns helper function
    INPUT: filename (string)
    OUTPUT: 1 if 'pair' is contained within filename string, 0 if not
    '''
    filename = filename.lower()
    if 'pair' in filename:
        return 1
    else:
        return 0


def find_relevant_rows(df):
    '''
    INPUT: Dataframe
    OUTPUT: Dataframe, only with rows that we have tagged as pair assignments
    '''
    column_name = 'filename'
    df[column_name] = df[column_name].apply(str)
    df['is_pair'] = pd.Series(
        df[column_name].apply(_pair_checker), index=df.index)
    return df.ix[df.is_pair == 1, :]


def _get_name_dict(df):
    '''
    INPUT: Dataframe with columns of names and github usernames
    OUTPUT: dictionary with keys of real names
    '''
    name_dict = defaultdict(str)
    for row in df.iterrows():
        if row[1]['Name']:
            name_dict[row[1]['Name']] = row[1]['github']
    return name_dict


def change_name_to_ghu(df1, df2):
    '''
    INPUT: Dataframe (df1) that has columns of names and github usernames, Dataframe (df2) of
    name data that needs to be converted
    OUTPUT: Modified df2 with names changed to github usernames
    '''
    name_dict = _get_name_dict(df1)
    name_list = name_dict.keys()
    for row in df2.iterrows():
        name1 = process.extractOne(row[1]['Name1'], name_list)[0]
        name2 = process.extractOne(row[1]['Name2'], name_list)[0]
        df2.set_value(row[0], 'Name1', name_dict[name1])
        df2.set_value(row[0], 'Name2', name_dict[name2])
        if row[1]['Name3']:
            name3 = process.extractOne(str(row[1]['Name3']), name_list)[0]
            df2.set_value(row[0], 'Name3', name_dict[name3])
    return df2


def filler(final_df, repo, name1, name2, name3=None):
    if not pair_length_df.loc[(pair_length_df.gh_username == name1) & (pair_length_df.repo_name == repo)].empty:
        temp = pair_length_df.loc[(pair_length_df.gh_username == name1) & (
            pair_length_df.repo_name == repo)]
        # In other file, set pair programming assignment value for user and
        # repo
        final_df.set_value(final_df.loc[final_df.github == name1].index, repo,
                            temp.file_length.mean())
        if temp.shape[0] != 1:
            # Set off warning if pair progamming submission consists of
            # multiple files
            print "Warning: {}: {} consists of multiple files".format(name1, repo)

    # Analogous logic to above if statement - note that we check partner
    # value, but assign to primary user
    elif not pair_length_df.loc[(pair_length_df.gh_username == name2) & (pair_length_df.repo_name == repo)].empty:
        temp = pair_length_df.loc[(pair_length_df.gh_username == name2) & (
            pair_length_df.repo_name == repo)]
        final_df.set_value(final_df.loc[final_df.github == name1].index, repo,
                          temp.file_length.mean())
        if temp.shape[0] != 1:
            # Set off warning if pair progamming submission consists of
            # multiple files
            print "Warning: {}: {} consists of multiple files".format(name2, repo)

    elif not pair_length_df.loc[(pair_length_df.gh_username == name3) & (pair_length_df.repo_name == repo)].empty:
        temp = pair_length_df.loc[(pair_length_df.gh_username == name3) & (
            pair_length_df.repo_name == repo)]
        final_df.set_value(final_df.loc[final_df.github == name1].index, repo,
                          temp.file_length.mean())
        if temp.shape[0] != 1:
            # Set off warning if pair progamming submission consists of
            # multiple files
            print "Warning: {}: {} consists of multiple files".format(name3, repo)

    else:
        print "Error: No pair programming values found for {}, repo: {}".format(name1, repo)


def _isNotNan(obj):
    '''
    Helper function that checks for non-missing values
    '''
    return obj == obj


def fill_lengths(pairdf, pair_lengthdf, final_df):
    '''
    INPUT: pairdf: dataframe with pair assignments for a given repo and cohort, pair_lengthdf: dataframe with scraped
    data from Github featuring the length of pair assignments, finaldf: dataframe with students as rows
    OUTPUT: modified finaldf with pair assignment lengths filled in
    '''
    for row in pairdf.iterrows():
        repo = row[1]['repo']
        name1 = row[1]['Name1']
        name2 = row[1]['Name2']
        if _isNotNan(row[1]['Name3']):
            name3 = row[1]['Name3']
            filler(final_df, repo, name1, name2, name3)
            filler(final_df, repo, name2, name1, name3)
            filler(final_df, repo, name3, name2, name1)
        else:
            filler(final_df, repo, name1, name2)
            filler(final_df, repo, name2, name1)
    # I don't think that I'll need to return anything here

def write_to_csv(df):
    df.to_csv('../final_sheet.csv')


if __name__ == "__main__":
    pair_df = grab_pair_assignment_data()
    pair_length_df = find_relevant_rows(grab_length_data(os.getcwd()))
    final_df = grab_final_data()
    pair_df = change_name_to_ghu(final_df, pair_df)
    print "*** Github names changed! ***"
    fill_lengths(pair_df, pair_length_df, final_df)
    print "*** Lengths inputted! ***"
    write_to_csv(final_df)
