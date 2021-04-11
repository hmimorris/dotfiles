var Win = null;

function onClose()
{
	Win = null;
}

function CreateWindow()
{
	Win = chrome.app.window.get("fileWin")
	if (Win != null){
		console.log("Window exists");
		return;
	}
	else{
		console.log("Window does not exist");
	}		

	var Options = {
		id:"fileWin", 
		bounds: {width: 10, height: 10},
		hidden: true		
	}
	chrome.app.window.create(
		"window.html", Options, 
		function(win) {
			Win = win;
			Win.hide();
		});
}

chrome.runtime.onInstalled.addListener(function() 
{
	CreateWindow();
});


chrome.app.runtime.onLaunched.addListener(function() 
{
	CreateWindow();
});


//-----------------------------------------------------------------------------

function onRecieveMessage(Message,Sender,Response)
{
	if (Message.command == "ping")
	{
		CreateWindow();
		Response({"result":"OK"});
	}
	
}

//-----------------------------------------------------------------------------

chrome.runtime.onMessageExternal.addListener(onRecieveMessage);
chrome.app.window.onClosed.addListener(onClose);