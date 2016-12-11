{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "#libraries!\n",
    "import requests\n",
    "import urllib\n",
    "import json\n",
    "from __future__ import division\n",
    "import math\n",
    "import csv"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "metadata": {
    "collapsed": true
   },
   "outputs": [],
   "source": [
    "#here is a function to get all the data for the different issue areas \n",
    "#the two input parameters are the issue area, and the name of the output csv file \n",
    "\n",
    "def legco(issue, file):\n",
    "    #create the url for access\n",
    "    base_url=\"http://app.legco.gov.hk/PolicyIssuesDB/odata/Vpolicy_issues_events?\"\n",
    "    # set response format\n",
    "    response_format=\".json\"\n",
    "    search_params = {\"$inlinecount\":\"allpages\",\n",
    "                     \"$filter\":\"substringof(\"+\"'\"+issue+\"'\"+\", issue_name_eng) eq true\"}  \n",
    "    r = requests.get(base_url+response_format, params=search_params)\n",
    "\n",
    "    response_text=r.text\n",
    "    data=json.loads(r.text)\n",
    "    \n",
    "    #the data I am interested in is nested in the 'value' key\n",
    "    docs = data['value']\n",
    "    \n",
    "    #format the text \n",
    "    formatted = []\n",
    "    for i in docs:\n",
    "        dic = {}\n",
    "        dic['area']=i['area_name_eng']\n",
    "        dic['issu'] = i['issue_name_eng']\n",
    "        dic['ctxt'] = i['context_name_eng']\n",
    "        dic['act']=i['event_action_eng']\n",
    "        dic['date']=i['event_date']\n",
    "        dic['summ']=i['event_summary_eng']\n",
    "        dic['key']=i['event_key']\n",
    "        formatted.append(dic)\n",
    "    #save the output into a .csv file \n",
    "    with open('/Users/Chuyue/Google Drive/Berkeley/Fall 2016/Academics/PS 239T/PS-239T-Final-Project/Data/LegCo/legco_'\n",
    "              + file + '.csv', 'w') as output_file:\n",
    "        dict_writer = csv.DictWriter(output_file, formatted[1].keys())\n",
    "        dict_writer.writeheader()\n",
    "        dict_writer.writerows(formatted)\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "metadata": {
    "collapsed": false
   },
   "outputs": [],
   "source": [
    "#get all the data on relevant issues \n",
    "legco(\"Press freedom\",\"prfm\" ) #press freedom\n",
    "legco(\"Electoral matters\", \"elema\") #electoral matters \n",
    "legco(\"Constitutional development\", \"condev\")\n",
    "legco(\"Application of certain provisions of the Prevention of Bribery Ordinance (Cap. 201) to the Chief Executive\", \n",
    "      \"chex\") #bribery of Chief Exec \n",
    "legco(\"System of accountability for principal officials\", \"acct\") #official accountability \n"
   ]
  }
 ],
 "metadata": {
  "anaconda-cloud": {},
  "kernelspec": {
   "display_name": "Python [default]",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.5.2"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 1
}
