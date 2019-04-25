import os
import geoip2.database
import re
import httpagentparser
import subprocess
import os
import argparse
import pandas as pd
import re
import urllib
import httpagentparser
from sklearn.feature_extraction.text import CountVectorizer

stop=['href','com','rel','@','http','https','all','i','me','my','myself','we','our','ours','ourselves','you',"you're","you've","you'll","you'd",'your','yours','yourself','yourselves','he','him','his','himself','she',"she's",'her','hers','herself','it',"it's",'its','itself','they','them','their','theirs','themselves','what','which','who','whom','this','that',"that'll",'these','those','am','is','are','was','were','be','been','being','have','has','had','having','do','does','did','doing','a','an','the','and','but','if','or','because','as','until','while','of','by','for','with','against','into','through','during','before','after','to','down','in','out','over','under','again','further','then','once','here','there','when','where','why','how','all','any','each','few','more','most','other','some','such','no','nor','not','only','own','same','so','than','too','very','s','t','can','will','just','don',"don't",'should',"should've",'now','d','ll','m','o','re','ve','y','ain','aren',"aren't",'couldn',"couldn't",'didn',"didn't",'doesn',"doesn't",'hadn',"hadn't",'hasn',"hasn't",'haven',"haven't",'isn',"isn't",'ma','mightn',"mightn't",'mustn',"mustn't",'needn',"needn't",'shan',"shan't",'shouldn',"shouldn't",'wasn',"wasn't",'weren',"weren't",'won',"won't",'wouldn',"wouldn't","nbsp"]


def url_decode(text):
    """
    UDF for url decode
    """
    if text is not None:
        return urllib.parse.unquote(text)
    else:
        return ''
    
def key_words(text):
    """
    UDF for keywords extraction
    """
    out=re.search('keyword=(.+?)&',text)
    if out:
        out=re.sub('%20|\+',' ',out.group(1))
        if '&' not in out: 
            return out
        else:
            return 'no_keywords'
    else:
        return 'no_keywords'
def creative(text):
    """
    UDF for creative extraction
    """
    out=re.search('.+creative=(.+?)&',text)
    if out:
        out=out.group(1)
        return out
    else:
        return 'no_creative'

def media(text):
    """
    UDF for media extraction
    """
    out=re.search('.+media=(.+?)&',text)
    if out:
        out=out.group(1)
        return out
    else:
        return 'no_media'
    
def camp(text):
    """
    UDF for campaign extraction
    """
    out=re.search('.+campaign=(.+?)&',text)
    if out:
        out=out.group(1)
        out=re.sub('%20|%2|\+',' ',out)
        return out
    else:

        return 'no_campain_found'
    
    
def location_text(text):
    """
    UDF for location
    """
    out=re.search('www.(\w+?)\.+',text)
    if out:
        return out.group(1)
    else: 
        return text
    
    
def ua_parser(text):
    """
    UDF for ua_string_parsing
    """
    browser='not_found'
    os='not_found'
    parse_data=httpagentparser.detect(text)
    if 'browser' in parse_data.keys():
        browser=parse_data['browser']['name']
    if 'os' in parse_data.keys():
        os=parse_data['os']['name']
    return (browser,os)

def get_loc(ip):
    """
    UDF for extracting information from IP
    """
    geo_resp=dict()
    resp=reader.city(ip)
    geo_resp['country']=resp.country.name
    geo_resp['city']=resp.city.name
    geo_resp['sub_div']=resp.subdivisions.most_specific.name
    return geo_resp

def text_process(text=''):
    if text is not None or len(text) !=0:
        text=re.sub('/|-',' ',text)
        text=text.split(' ')
        text=[i for i in text if i not in stop]
        text=' '.join(text)
        return text
    else:
        return ''

def save_xls(dict_df, path):
    """
    Save a dictionary of dataframes to an excel file, with each dataframe as a seperate page
    """

    writer = pd.ExcelWriter(path)
    for key in dict_df:
        dict_df[key].to_excel(writer, '%s' % key)

    writer.save()



if __name__=="__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument('path',help='input_file_path')
    args=parser.parse_args()
    try:
        """
        installing geoip and its location database which will require for UDF get_loc
        """
        os.system('pip3 install geoip2')
        os.system('wget https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz')
        os.system('tar -xzf GeoLite2-City.tar.gz')
        """
        Reading input file which has been passed with arg_parser 
        """
        if args.path.endswith('.xls') or args.path.endswith('.xlsx'):
            text_data=pd.read_excel(args.path)
        else:
            text_data=pd.read_csv(args.path)
            
        text_data=pd.read_csv(args.path)
        text_data.fillna('na_values',inplace=True)
        text_data['location query']=text_data['location query'].apply(url_decode)
        text_data['location_keywords']=text_data['location query'].apply(key_words)
        text_data['location_creative']=text_data['location query'].apply(creative)
        text_data['location_camp']=text_data['location query'].apply(camp)
        text_data['media_text']=text_data['location query'].apply(media)
        
        text_data.timestamp = pd.to_datetime(text_data.timestamp).dt.date
        text_data.location=text_data.location.apply(location_text)
        
        text_data['referral location']=text_data['referral location'].apply(lambda x: 'google' if 'www.google' in x else x)
        
        location_keywords_data=pd.DataFrame(text_data.groupby(['location','timestamp','referral location']).location_keywords.agg(['count','min','max']))
        
        location_camp_data=pd.DataFrame(text_data.groupby(['location','timestamp','referral location']).location_camp.agg(['count','min','max']))
        
        location_creative_data=pd.DataFrame(text_data.groupby(['location','timestamp','referral location']).location_creative.agg(['count','min','max']))
        
        ua_result=text_data.ua_string.apply(ua_parser)
        text_data['ua_browser']=[i[0] for i in ua_result]
        text_data['ua_os']=[i[1] for i in ua_result]
        ua_string_data=pd.DataFrame(text_data.groupby(['location','ua_os']).ua_browser.agg(['count','max']))
        
        """
        Loading database from current directory
        """
        db_dir=[file for file in os.listdir('./') if file.startswith('GeoLite') and not file.endswith('.gz')][0]
        db_file=[file for file in os.listdir('./{}'.format(db_dir)) if file.endswith('.mmdb')][0]
        reader_path='./{}'.format(os.path.join(db_dir,db_file))
        
        """
        Creating reader object
        """
        reader=geoip2.database.Reader(reader_path)
        geo_out=text_data.geo_ip.apply(get_loc)
        reader.close()
        text_data['geo_country']=[i['country'] for i in geo_out]
        text_data['geo_city']=[i['city'] for i in geo_out]
        text_data['geo_sub_div']=[i['sub_div'] for i in geo_out]
        
        geo_ip_data=pd.DataFrame(text_data.groupby(['geo_country','geo_city']).geo_sub_div.agg(['count','max']))
        
        
        """
        I have traine BOW for capturing top frequent words(after removing stop words) in location path
        """
        loc_path=text_data['location path'].apply(text_process)
        cv=CountVectorizer(ngram_range=(1,3))
        cv.fit(loc_path)
        sorted_x = sorted(cv.vocabulary_.items(), key=lambda kv: kv[1],reverse=True)
        """
        I have taken only top 30 trigrams.
        """
        sorted_x=[i for i in sorted_x if len(i[0].split(' '))>2]
        keys=[i[0] for i in sorted_x[:30]]
        vals=[i[1] for i in sorted_x[:30]]

        trigram_count=pd.DataFrame({'trigrams_location_path':keys,'count_across_document':vals})
        
        """
        creating dictionary for dataframes that has been created during above aggregation
        """
        
        dict_df={'location_camp_data':location_camp_data,'location_creative_data':location_creative_data,
                 'location_keywords_data':location_keywords_data,'trigram_count':trigram_count,
                 'ua_string_data':ua_string_data,'geo_ip_data':geo_ip_data}
        
        print('writing output in current directory, file will be in xls format.')
        save_xls(dict_df,path='./samp_output.xls')
        
    except Exception as e:
        print('Following error occured:>>\n',e)

    
    
    
    
    
    