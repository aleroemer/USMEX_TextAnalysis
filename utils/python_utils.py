"""
Utils for USMEX_TextAnalysis done in Python. 

In a cell in your notebook, run:
%run '{path}/hugos/utils/graph_utils.py'
"""

import flair #'!pip3 install flair' if you haven't installed it


flair_sentiment = flair.models.TextClassifier.load('en-sentiment')


def analyze_paragraphs_sentiment(df):
    """
    Analyzes sentiment for paragraphs in a DataFrame using the Flair library.

    Args:
        df (pd.DataFrame): DataFrame containing paragraphs to analyze.

    Returns:
        pd.DataFrame: The input DataFrame with three additional columns:
            'sentiment_label' (str): The sentiment label for each paragraph 
              ('POSITIVE' or 'NEGATIVE').
            'sentiment_label_is_positive' (int): 1 for POSITIVE, 0 for NEGATIVE
            'sentiment_confidence' (float): The confidence score associated with the sentiment.
    """
    label_lst = []
    confidence_lst = []
    
    # Iterate through each row in the DataFrame
    for row in range( len(df) ):
        # Create a Sentence object using the text from the current row
        current_paragraph = flair.data.Sentence(df['text_paragraph'][row])
        
        # Predict the sentiment using Flair
        flair_sentiment.predict(current_paragraph)
        
        # Extract the sentiment label and confidence from the Sentence's attributes
        label_in_dict = current_paragraph.to_dict()['all labels'][0]
        label_lst.append(label_in_dict['value'])  # Append the sentiment label
        confidence_lst.append(label_in_dict['confidence'])  # Append the confidence score
        
    df['sentiment_label'] = label_lst
    df['sentiment_label_is_positive'] = (df.sentiment_label == 'POSITIVE').astype(int)
    df['sentiment_confidence'] = confidence_lst
        
    return df
