# Initialize the OpenAI client ####
API_KEY = os.getenv('OPENAI_API_KEY')
client = OpenAI(api_key=API_KEY)


# Load data ####
codebook = pd.read_excel(os.path.join(raw_aspire_north_data_dir, 'Consumer_202504_a.xls'), skiprows=1)


# Use prompt to classify data types ####
# Create prompt
def create_prompt(field_name, long_description, field_type, field_values):

    prompt = f'''
    You are a data assistant. Your task is to figure out the field type so that I can feed it in when I run the regression analysis. Always provide the output in a standardized JSON format. I want you to choose from the following options:
    - categorical: This field contains categories or labels, such as names, types, or classifications.
    - ordinal: This field contains ordered categories, where the order matters but the exact difference between categories is not defined.
    - numerical: This field contains numbers that can be used in calculations, such as counts, measurements, or scores.
    - text: This field contains free-form text or descriptions that do not fit into the other categories.
    - date: This field contains dates or timestamps.
    - identifier: This field contains unique identifiers or keys that are used to distinguish records.

    Field Name: {field_name}
    Long Description: {long_description}
    Field Type: {field_type}
    Field Values: {field_values}

    Output:
    Provide the data type in the following JSON format:
    {{
      'data_type': '<your guess of the data type (e.g., categorical, numerical, text)>',
    }}

    Examples:

    Example 1:
    Field Name: Homeowner: Probability Model
    Long Description: Homeowner Probability (HOP) predicts the likelihood that a Living Unit owns their home. Homeowner Probability scores rank single family dwelling units using individual, living unit and area level demographics. The higher scores indicate the strongest probability of ownership. Known homeowners, that being confirmed owner occupied properties, are depicted with scores of 101-102.
    Field Type: AN
    Field Values: 000=Renter,001=Probability of Homeownership,002=Probability of Homeownership,003=Probability of Homeownership,004=Probability of Homeownership,005=Probability of Homeownership,006=Probability of Homeownership,007=Probability of Homeownership,008=Probability of Homeownership,009=Probability of Homeownership,010=Probability of Homeownership,011=Probability of Homeownership,012=Probability of Homeownership,013=Probability of Homeownership,014=Probability of Homeownership,015=Probability of Homeownership,016=Probability of Homeownership,017=Probability of Homeownership,018=Probability of Homeownership,019=Probability of Homeownership,020=Probability of Homeownership,021=Probability of Homeownership,022=Probability of Homeownership,023=Probability of Homeownership,024=Probability of Homeownership,025=Probability of Homeownership,026=Probability of Homeownership,027=Probability of Homeownership,028=Probability of Homeownership,029=Probability of Homeownership,030=Probability of Homeownership,031=Probability of Homeownership,032=Probability of Homeownership,033=Probability of Homeownership,034=Probability of Homeownership,035=Probability of Homeownership,036=Probability of Homeownership,037=Probability of Homeownership,038=Probability of Homeownership,039=Probability of Homeownership,040=Probability of Homeownership,041=Probability of Homeownership,042=Probability of Homeownership,043=Probability of Homeownership,044=Probability of Homeownership,045=Probability of Homeownership,046=Probability of Homeownership,047=Probability of Homeownership,048=Probability of Homeownership,049=Probability of Homeownership,050=Probability of Homeownership,051=Probability of Homeownership,052=Probability of Homeownership,053=Probability of Homeownership,054=Probability of Homeownership,055=Probability of Homeownership,056=Probability of Homeownership,057=Probability of Homeownership,058=Probability of Homeownership,059=Probability of Homeownership,060=Probability of Homeownership,061=Probability of Homeownership,062=Probability of Homeownership,063=Probability of Homeownership,064=Probability of Homeownership,065=Probability of Homeownership,066=Probability of Homeownership,067=Probability of Homeownership,068=Probability of Homeownership,069=Probability of Homeownership,070=Probability of Homeownership,071=Probability of Homeownership,072=Probability of Homeownership,073=Probability of Homeownership,074=Probability of Homeownership,075=Probability of Homeownership,076=Probability of Homeownership,077=Probability of Homeownership,078=Probability of Homeownership,079=Probability of Homeownership,080=Probability of Homeownership,081=Probability of Homeownership,082=Probability of Homeownership,083=Probability of Homeownership,084=Probability of Homeownership,085=Probability of Homeownership,086=Probability of Homeownership,087=Probability of Homeownership,088=Probability of Homeownership,089=Probability of Homeownership,090=Probability of Homeownership,091=Probability of Homeownership,092=Probability of Homeownership,093=Probability of Homeownership,094=Probability of Homeownership,095=Probability of Homeownership,096=Probability of Homeownership,097=Probability of Homeownership,098=Probability of Homeownership,099=Probability of Homeownership,100=Probability of Homeownership,101=Known Homeowner,102=Condo Owner,999=Unknown or not scored,blank=Blank,
    Output:
    {{
      'data_type': 'numerical',
    }}

    Example 2:
    Field Name: Person 4: PoliticalPersona I1
    Long Description: PoliticalPersonas Segments provide a detailed understanding of key voter segments, demographics, responsiveness to various media platforms, and attitudes and opinions on important political issues and social trends.
    Field Type: AN
    Field Values: 00=Uncoded,01=Unconnected and Unregistered,02=Informed But Unregistered,03=Super Democrats,04=Left Out Democrats,05=Conservative Democrats,06=On-the-Fence Liberals,07=Green Traditionalists,08=Mild Republicans,09=Uninvolved Conservatives,10=Ultra Conservatives,blank=Blank,
    Output:
    {{
      'data_type': 'categorical',
    }}

    Example 3:
    Field Name: Person 5: Ethnic - Ethnic
    Long Description: Based on a comprehensive predictive name analysis process which identifies ethnicity.
    Field Type: char
    Field Values: 00=Unknown,01=English,02=Scottish,03=Danish,04=Swedish,05=Norwegian,06=Finnish,07=Icelandic,08=Dutch,09=Belgian,10=German,11=Austrian,12=Hungarian,13=Czech,14=Slovak,15=Irish,16=Welsh,17=French,18=Swiss,19=Italian,2=Scottish,20=Hispanic,21=Portuguese,22=Polish,23=Estonian,24=Latvian,25=Lithuanian,26=Ukrainian,27=Georgian,28=Byelorussian,29=Armenian,30=Russian,31=Turkish,32=Kurdish,33=Greek,34=Persian,35=Moldovan,36=Bulgarian,37=Romanian,38=Albanian,39=Native American,40=Slovenian,41=Croat,42=Serb,43=Bosniak,44=Azerbaijani,45=Kazakh,46=Afghan,47=Pakistani,48=Bengali,49=Indonesian,50=Asian Indian,51=Burmese (Myanmar),52=Mongolian,53=Chinese,55=Taiwanese,56=Korean,57=Japanese,58=Thai,59=Malaysian,60=Laotian,61=Khmer,62=Vietnamese,63=Sri Lankan,64=Uzbek,65=Hmong,66=Jewish,67=Aleut,68=Hebrew,70=Arab,71=Brazilian,72=Turkmen,73=Tajik,74=Kirghiz,75=Saudi,76=Iraqi,77=Libyan,78=Egyptian,79=Rwandan,7A=Hindu(retired 2022),7B=Djiboutian,7C=Manx,7D=Telugan(retired 2022),7E=Nepalese,7F=Samoan,7G=Mauritanian,7H=Inuit,7M=Trinidadian,7N=Southern African,80=Tongan,81=Senegalese,82=Malawian,83=South sudanese,84=Moroccan,85=African American,86=Kenyan,87=Nigerian (Nigeria),88=Ghanaian,89=Zambian,8A=Congolese,8B=Central african republic,8C=Togolese,8D=Bahraini,8E=Qatari,8F=Guyanese,8G=Tibetan,8H=Fijian,8I=Swazi,8J=Zulu,8K=Xhosa,8L=Basotho(retired 2022),8M=Afrikaner,8N=Liberian,8O=Comoran,8P=Beninese,8Q=Burkinabe (Burkina Faso),8R=Nigerian (Niger),8S=Akan,8T=Swahili,8U=Haitian,8V=Malian,8W=Jamaican,8X=Hausa,8Y=African Continental,90=Zaire(retired 2022),91=Surinamese,92=Mozambican (Mozambique),93=Ivorian (Ivory Coast),94=Bhutanese,95=Ethiopian,96=Ugandan,97=Botswana,98=Cameroonian,99=Zimbabwean,9A=Namibian,9B=Burundi,9C=Tanzanian,9D=Gambian,9E=Somali,9F=Macedonian,9G=Chadian,9H=Gabonese,9I=Angolan,9J=Chechen,9K=Igbo,9L=Yoruba,9M=Algerian,9N=Filipino,9O=Sotho (Lesotho),9P=Tunisian,9Q=Hawaiian,9R=Malagasy (Madagascar),9S=Basque(retired 2022),9T=Sierra leonean,9U=Kuwaiti,9V=Yemeni,9W=Guinea-bissau,9X=Papua new guinean,9Y=Equatorial guinean,9Z=Syrian,A1=African American (Arabic),A2=African American (Arabic),A3=African American (Arabic),A4=African American (Arabic),A5=African American (Arabic),blank=Null,D1=African American (Dutch),D2=African American (Dutch),D3=African American (Dutch),D4=African American (Dutch),D5=African American (Dutch),E1=African American (English),E2=African American (English),E3=African American (English),E4=African American (English),E5=African American (English),F1=African American (French),F2=African American (French)(retired 2023),F3=African American (French)(retired 2023),F4=African American (French),F5=African American (French),I1=African American (Irish),I2=African American (Irish),I3=African American (Irish),I4=African American (Irish),I5=African American (Irish),S1=African American (Scotch),S2=African American (Scotch),S3=African American (Scotch),S4=African American (Scotch),S5=African American (Scotch),U1=African American (Unknown),U2=African American (Unknown),U3=African American (Unknown),U4=African American (Unknown),U5=African American (Unknown),UC=Uncodable records,W1=African American (Welsh),W2=African American (Welsh),W3=African American (Welsh),W4=African American (Welsh),W5=African American (Welsh),ZZ=Multi-Ethnic,
    Output:
    {{
      'data_type': 'categorical'
    }}

    Example 4:
    Field Name: Latitude
    Long Description: Enrichment uses the Compass display format which has N=North and S=South in the last position of the field.  Universal Enrichment uses the Signed degree display, which uses - sign to indicate South latitude.
    Field Type: AN
    Field Values: null
    Output:
    {{
      'data_type': 'numerical',
    }}
    '''    
    return prompt

# Define the classification function
def extract_data_type(field_name, long_description, field_type, field_values):
    
    prompt = create_prompt(field_name, long_description, field_type, field_values)
    
    # Call the chat completion API
    response = client.chat.completions.create(
        model='gpt-4.1-mini',
        messages=[
            {
                'role': 'user',
                'content': prompt
            }
        ]
    )
    
    response_text = response.choices[0].message.content
    text = response_text.strip('```').replace('json\n', '', 1)
    data = json.loads(text)
    
    return data

# Execute!
data_type_list = []

for index, row in codebook.iterrows():
    try:
        response = extract_data_type(row['Field Name'], row['Long Description'], row['Field Type'], row['Field Values'])
        data_type_list.append(response['data_type'])
        print(response)
        print()

    except:
        data_type_list.append('null')
        print(row)
        print()

codebook['data_type'] = data_type_list

# Save the codebook with data types
codebook.to_csv(os.path.join(working_data_dir, 'pre-processed', 'aspire_north_codebook.csv'), index=False)
