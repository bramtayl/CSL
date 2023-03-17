# install pandas and selenium with pip
import pandas
import re
from selenium import webdriver
from selenium.webdriver.common.by import By
import time

# the last ratio index assigned is 34466
# so if we go through 34500 we will get them all
NUMBER_OF_CHUNKS = 345
KIND = "ratios"

# kind is a ratio, method, or weight
def get_url(kind, index):
    return "https://cear.tuftsmedicalcenter.org/registry/" + kind + "/" + str(index)

# the title of the study
def get_title(driver):
    return driver.find_element(By.CSS_SELECTOR, ".align-end").text

# the abstract of the study
def get_abstract(driver):
    return driver.find_element(By.CSS_SELECTOR, ".articleContent__body").text

def split_fill(text):
    # split the two lines
    items = text.split("\n")
    if (len(items) == 1):
        # if just one line, the second line is empty
        return [text, ""]
    else:
        return items

# details about the article
# the key in a separate tag, but the value is not, so they are difficult to separate
# do it ourselves
def get_article_details(driver):
    return (split_fill(element.text) for element in driver.find_elements(By.CSS_SELECTOR, ".articleContent__detail"))

# details about the figure from the article
def get_figure_details(driver):
    return zip(
        # keys
        (element.text for element in driver.find_elements(By.CSS_SELECTOR, ".mr-4")),
        # values
        (element.text for element in driver.find_elements(By.CSS_SELECTOR, ".articleDetails__detailData"))
    )

def begin_downloading(driver, kind, sleep_time = 2, start_index = 1):
    driver.get(get_url(kind, start_index))
    # wait for the page to load
    time.sleep(sleep_time)

    columns_dict = {}
    # create an list with just one item for each column
    columns_dict["index"] = [start_index]
    columns_dict["title"] = [get_title(driver)]
    columns_dict["abstract"] = [get_abstract(driver)]
    for (column_name, value) in get_article_details(driver):
        columns_dict[column_name] = [value]
    for (column_name, value) in get_figure_details(driver):
        columns_dict[column_name] = [value]
    
    return columns_dict

# save every 100 indices (a chunk) so we can stop and resume
def download_chunk(columns_dict, driver, kind, chunk_number, sleep_time = 2, chunk_size = 100):
    # clear all the old columns
    for (_, column) in columns_dict.items():
        column.clear()

    # if the chunk number is 2, then we are looking for 101-200
    start_index = (chunk_number - 1) * chunk_size + 1
    end_index = chunk_number * chunk_size
    for index in range(start_index, end_index + 1):
        driver.get(get_url(kind, index))
        time.sleep(sleep_time)
        # there are some small gaps where indices are not assigned
        # for in an index in a gap, there will be a bunch of empty cells
        # so if there are any empty cells, skip the index
        if (len(driver.find_elements(By.CSS_SELECTOR, ".v-skeleton-loader__bone")) > 0):
            print(str(index) + " not found")
        else:
            # add new data at the end of every column
            columns_dict["index"].append(index)
            columns_dict["title"].append(get_title(driver))
            columns_dict["abstract"].append(get_abstract(driver))
            for (column_name, value) in get_article_details(driver):
                columns_dict[column_name].append(value)
            for (column_name, value) in get_figure_details(driver):
                columns_dict[column_name].append(value)
            # print the indices to keep track
            print(str(index) + " found")
    # TODO: reuse dataframe?
    pandas.DataFrame(columns_dict).to_csv(
        "data/ratios/" +
        str(start_index) +
        "-" +
        str(end_index) +
        ".csv",
        index = False
    )
    return

# to use this driver, geckodriver needs to be on your path
driver = webdriver.Firefox()

columns_dict = begin_downloading(driver, KIND)

for chunk_number in range(0, NUMBER_OF_CHUNKS):
    download_chunk(columns_dict, driver, KIND, chunk_number)

driver.close()