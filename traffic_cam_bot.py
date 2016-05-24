# This bot was written for my team in Slack. A user can direct message "tuas" or
# "woodlands" to the bot. The bot would then use data.gov.sg API to pull the
# latest snapshot of the causeway and send it to the user.

from slackclient import SlackClient
from datetime import datetime, timedelta
import requests, time


BOT_TOKEN    = "BOT_TOKEN"
DATA_GOV_KEY = "DATA_GOV_KEY"
# traffic_cam's ID
TRAFFIC_CAM_ID = "TRAFFIC_CAM_ID"


def get_image_info(cameras, camera_id):
    """Helper function to get image URL & timestamp based on camera id."""
    for cam in cameras:
        if cam["camera_id"] == camera_id:
            return (cam["timestamp"], cam["image"])

    return "Camera {} not found!".format(str(camera_id))

def get_images(text, time):
    """Calls data.gov.sg traffic-images API to get image URLs for the requested
    location at the requested time.
    Returns list of (Timestamp, Image URL)s.
    In the event of an error, returns a string."""
    # make request to data.gov.sg
    payload = {"date_time": time.strftime("%Y-%m-%dT%H:%M:%S")}
    headers = {"api-key": DATA_GOV_KEY}
    r = requests.get("https://api.data.gov.sg/v1/transport/traffic-images",
                     params=payload, headers=headers)

    if r.status_code != 200:
        return "API call failed! Error {}".format(str(r.status_code))
    else:
        cameras = r.json()["items"][0]["cameras"]
        if text == "tuas":
            return [get_image_info(cameras, 4703), get_image_info(cameras, 4713)]

        elif text == "woodlands":
            return [get_image_info(cameras, 2701), get_image_info(cameras, 2702)]
        else:
            return "Wrong text!"


def process_msg(sc, event):
    text    = event["text"].lower()
    channel = event["channel"]

    # get current singapore time
    time = datetime.utcnow() + timedelta(hours=8)

    if text in {"tuas", "woodlands"}:
        sc.api_call("chat.postMessage", channel=channel, as_user="true",
                    text="Getting traffic image of {}...".format(text.title()))

        API_result = get_images(text, time)

        # print result of API call
        if type(API_result) == str:
            sc.api_call("chat.postMessage", channel=channel, as_user="true",
                        text= API_result)
        else:
            for cam in API_result:
                if type(cam) == tuple and len(cam) == 2:
                    sc.api_call("chat.postMessage", channel=channel, as_user="true",
                            text= "{}: {}".format(cam[0], cam[1]))
                else:
                    sc.api_call("chat.postMessage", channel=channel, as_user="true",
                            text= cam)
    # user keyed in invalid input
    else:
        sc.api_call("chat.postMessage", channel=channel, as_user="true",
                text="Did you type something wrong? I only accept 'Tuas' and 'Woodlands' as input at the moment.")


def main():
    # create slackclient instance
    sc = SlackClient(BOT_TOKEN)

    # connect to slackclient
    if sc.rtm_connect():
        while True:
            new_events = sc.rtm_read()

            for event in new_events:
                # print(event)  # for testing purposes

                # if a message was sent (not by traffic_cam),
                # then process the message
                if event["type"] == "message" and "user" in event and \
                   event["user"] != TRAFFIC_CAM_ID:
                    process_msg(sc, event)

            # sleep for a second
            time.sleep(1)
    else:
        print("Couldn't connect to slack")

if __name__ == "__main__":
    main()
