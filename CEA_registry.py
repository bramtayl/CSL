# install pandas and selenium with pip
from pandas import DataFrame

from selenium.webdriver import FirefoxService, Firefox, FirefoxOptions
from selenium.webdriver.common.by import By
from time import sleep
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support.expected_conditions import presence_of_element_located

# the last ratio index assigned is 42414 as of May 10, 2024
# so if we go through 42500 we will get them all
# NUMBER_OF_CHUNKS = 42500
NUMBER_OF_CHUNKS = 425
SLEEP_TIME = 10
CHUNK_SIZE = 100

def wait_for_driver(driver, sleep_time):
    WebDriverWait(driver, sleep_time).until(
        presence_of_element_located((By.CSS_SELECTOR, "p.articleDetails__disclaimer"))
    )

def get_url(index):
    return "https://cear.tuftsmedicalcenter.org/registry/ratios/" + str(index)

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

def begin_downloading(driver, sleep_time = SLEEP_TIME, start_index = 1):
    driver.get(get_url(start_index))
    # wait for the page to load
    wait_for_driver(driver, sleep_time)

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
def download_chunk(columns_dict, driver, chunk_number, sleep_time = SLEEP_TIME, chunk_size = CHUNK_SIZE):
    # clear all the old columns
    for (_, column) in columns_dict.items():
        column.clear()

    # if the chunk number is 1, then we are looking for 101-200
    start_index = chunk_number * chunk_size + 1
    end_index = (chunk_number + 1) * chunk_size
    for index in range(start_index, end_index + 1):
        print(index)
        driver.get(get_url(index))
        try:
            wait_for_driver(driver, sleep_time)
        except TimeoutException:
            continue

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

    DataFrame(columns_dict).to_csv(
        "data/ratios/" +
        str(start_index) +
        "-" +
        str(end_index) +
        ".csv",
        index = False
    )
    return

options = FirefoxOptions()
options.headless = True
driver = Firefox(
    service = FirefoxService(executable_path = "/snap/bin/geckodriver"),
    options = options
)
sleep(SLEEP_TIME)
columns_dict = begin_downloading(driver)

for chunk_number in range(NUMBER_OF_CHUNKS):
    download_chunk(columns_dict, driver, chunk_number)

driver.close()
