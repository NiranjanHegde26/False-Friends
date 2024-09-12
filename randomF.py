import pandas as pd

# Read the CSV file
df = pd.read_csv('VST.csv')
ans = []
def get_selected_option(row):
    answer = row['Answer']
    if pd.notna(answer):  # Check if Answer is not NaN
        option_column = f'Option{int(answer)}'
        if option_column in row.index:
            ans.append(row[option_column])
            return row[option_column]
    return None  # Return None if no valid option is found

# Apply the function to create a new column with the selected option

    
    
abstract = 103
intro = 648
method = 82 + 195 + 127 + 125 + 43 + 97 + 92
result = 98 + 44 + 201 + 44 + 160 + 24 + 234 + 54 + 163 + 27 + 26
discussion = 372
print(abstract + intro + method + result + discussion, result, method) 
