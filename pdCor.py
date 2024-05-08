import sys
import dcor
import math
import numpy
import numpy as np
import pandas as pd
from scipy.stats import norm


def col_str_to_int(header, text):
    """
    Returns the position index of a specific column in a given header.
    Returns -1 if the column is not found.

            Parameters:
                    header (list): The header where to search the column
                    text (str): A string representing the column to be searched

            Returns:
                    column_index (int): The index of the column in the given header
    """

    column_index = -1

    # Given text is already a digit
    if str(text).lstrip("-").isdigit():
        # Check if digit is in a valid range
        if -1 < int(text) < len(header):
            column_index = int(text)
    else:
        text = text.strip()
        # Check if given text is the exact name of a column
        for i in range(len(header)):
            if text == header[i]:
                column_index = i

        # Proceed with alphabetical order ('A'=0,'B'=1,...,'AA'=26,...)
        if column_index == -1:
            result = 0
            for i in range(len(text)):
                result += int(((ord(text[i].upper())) - 64) * math.pow(26, len(text) - i - 1))
            if 0 < result < (len(header) + 1):
                column_index = result - 1

    return column_index


def fill_empty(data, head, start):
    for i in range(len(head) - start):

        column = head[i + start]

        smallest = float('inf')
        for x in data[column]:

            if smallest >= x != 0:
                smallest = x

        for k in range(len(data[column])):
            if (type(data.iloc[k, i + start]) != numpy.float64 and type(data.iloc[k, i + start]) != numpy.int) \
                    or pd.isna(data.iloc[k, i + start]):
               #MCC no imputation data.iloc[k, i + start] = smallest / 5
            if data.iloc[k, i + start] == 0:
                data.iloc[k, i + start] = smallest / 5

    return data


def pd_cor(data, i, j):
    return (-data[i][j]) / (math.sqrt(abs(data[i][i] * data[j][j])))


def p_val(cor, n, m):
    fisher = (math.log(abs((1 + cor) / (1 - cor)))) / 2
    cdf = norm.cdf(math.sqrt(n - (m - 2) - 3) * fisher)
    return 1 - cdf


def main(input_path, start_col_str='A', start_row=-1, end_row=-1, p_val_tol=0.01):
    if input_path[input_path.index('.') + 1:] == "xlsx":
        df = pd.read_excel(input_path)
    else:
        df = pd.read_csv(input_path)

    header = list(df.columns.values)
    start_col = col_str_to_int(header, start_col_str)
    df = fill_empty(df, header, start_col)

    if 1 < start_row <= len(df) + 1:
        start_row -= 2
    else:
        start_row = 0

    if 1 < end_row <= len(df) + 1:
        end_row -= 2
    else:
        end_row = len(df) - 1

    if start_row >= end_row:
        start_row = 0
        end_row = len(df) - 1

    df2 = pd.DataFrame(columns=header[start_col:])
    for i in range(len(header) - start_col):
        current_row = [0.0 for _ in range(len(header) - start_col)]
        for j in range(len(header) - start_col):
            new_i = i + start_col
            new_j = j + start_col

            x = np.array(list(df.iloc[:, new_i])[start_row: end_row + 1])
            y = np.array(list(df.iloc[:, new_j])[start_row: end_row + 1])

            d_cov = dcor.distance_covariance(x, y)
            current_row[j] = float(d_cov)

        df2.loc[i] = current_row

    data = df2.to_numpy(copy=True)

    if (end_row - start_row + 1) < 2 * (len(header) - start_col):
        print("Not enough samples")
        data = np.linalg.pinv(data)
    else:
        try:
            data = np.linalg.inv(data)
        except np.linalg.LinAlgError:
            print("Singular Matrix Error")
            data = np.linalg.pinv(data)

    result1 = [[0.0] * (len(header) - start_col + 1) for _ in range(len(header) - start_col + 1)]
    result2 = [[0.0] * (len(header) - start_col + 1) for _ in range(len(header) - start_col + 1)]

    for i in range(len(header) - start_col):
        result1[0][i + 1] = header[i + start_col]
        result1[i + 1][0] = header[i + start_col]
        result2[0][i + 1] = header[i + start_col]
        result2[i + 1][0] = header[i + start_col]

    for i in range(len(header) - start_col):
        for j in range(i, len(header) - start_col):

            if i == j:
                result1[i + 1][j + 1] = -1.0
                result2[i + 1][j + 1] = 0.0
                result1[j + 1][i + 1] = -1.0
                result2[j + 1][i + 1] = 0.0
            else:

                pd_correlation = pd_cor(data, i, j)
                p_value = p_val(pd_correlation, end_row - start_row + 1, len(header) - start_col)

                if p_value > p_val_tol:
                    result1[i + 1][j + 1] = 0.0
                    result2[i + 1][j + 1] = p_value
                    result1[i + 1][j + 1] = 0.0
                    result2[j + 1][i + 1] = p_value
                else:
                    result1[i + 1][j + 1] = pd_correlation
                    result2[i + 1][j + 1] = p_value
                    result1[j + 1][i + 1] = pd_correlation
                    result2[j + 1][i + 1] = p_value

    return result1, result2


args = sys.argv
(pd_cors, p_values) = main(args[1], args[3], int(args[4]), int(args[5]), float(args[6]))
print(p_values)

pd_cors = pd.DataFrame(pd_cors)
p_values = pd.DataFrame(p_values)

with pd.ExcelWriter(args[2]) as writer:
    pd_cors.to_excel(writer, sheet_name="Partial Distance Correlation")
    p_values.to_excel(writer, sheet_name="P-Values")
