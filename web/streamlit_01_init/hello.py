import streamlit as st

# Title
st.title("Hello, Streamlit with UV!")

# Input and Output
name = st.text_input("Enter your name:")
if name:
    st.write(f"Hello, {name}! Welcome to the app.")
