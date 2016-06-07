'use strict';

var Utils = {
	ofCatalogId : function(cata_id) {
		var inx = cata_id.indexOf("cata_");
		if (inx == -1) return cata_id;
		else return cata_id.substr(inx + "cata_".length);
	},

	toCatalogId : function(id) {
		return "cata_" + id;
	}
};

function Event (sender) {
	this._sender = sender;
	this._listeners = [];
}

Event.prototype = {
	register : function(f) {
		this._listeners.push(f);
	},
	notify : function(args) {
		var i = 0;
		for (; i < this._listeners.length; ++i) {
			this._listeners[i](this._sender, args);
		}
	}
};


function XHR() {
	this._root = "http://127.0.0.1:8081";
	this._review = "http://127.0.0.1:8443";
	this._catalog = "http://127.0.0.1:8088";

	this._get = function (url, handler) {
		var xhr = new XMLHttpRequest();
		xhr.onreadystatechange = function() {
			if (xhr.readyState === 4 && handler !== undefined)
				handler(url, xhr.status, xhr.responseText); 
		};

		xhr.open('GET', url, true);
		xhr.send();
	};

	this._post = function(url, data, handler) {
		var xhr = new XMLHttpRequest();
		xhr.onreadystatechange = function() {
			if (xhr.readyState === 4 && handler !== undefined)
				handler(url, xhr.status, xhr.responseText);
		};

		xhr.open('POST', url, true);
		xhr.send(JSON.stringify(data));
	};

	this._handler = function(expected, success, failure) {
		return function(url, status, text) {
			if (status === expected) {
				if (success !== undefined) success(text);
				else console.log(status + " " + text);
			} else {
				if (failure !== undefined) failure(url, status, text);
				else console.log("Fail: " + url + " " + status + " " + text);
			}
		};
	};
}

XHR.prototype = {
	create_review : function(r, success, failure) {
		
		var url = this._review + "/create/" + r.id;

		var handler = this._handler(200, success, failure);

		this._post(url, r, handler);
	},

	update_review : function(r, success, failure) {

		var url = this._review + "/update/" + r.id;

		var handler = this._handler(200, success, failure);

		this._post(url, r, handler);
	},

	remove_review : function(id, success, failure) {

		var url = this._review + "/delete/" + id;

		var handler = this._handler(200, success, failure);

		this._post(url, {}, handler);
	},

	read_review : function(id, success, failure) {

		var url = this._review + "/read/" + id;

		var handler = this._handler(200, success, failure);

		this._get(url, handler);
	},

	list_reviews : function(success, failure) {

		var url = this._review + "/list";

		var handler = this._handler(200, success, failure);

		this._get(url, handler);
	},

	search_movie : function(title, success, failure) {

		var url = this._root + "/title/" + encodeURIComponent(title);

		var handler = this._handler(200, success, failure);

		this._get(url, handler);
	},

	sync_catalog_review: function(success, failure) {

		 var url = this._catalog + "/review/sync";

		 var handler = this._handler(200, success, failure);

		 this._post(url, {}, handler);
	},

	update_catalog_users : function(success, failure) {

		var url = this._catalog + "/review/users";

		var handler = this._handler(200, success, failure);

		this._get(url, handler);
	},

	upload_review : function(id, success, failure) {

		var url = this._catalog + "/review/read/meta/" + id;

		var handler = this._handler(200, success, failure);

		this._post(url, {}, handler);
	},

	review_delegate : function(data, success, failure) {

		var url = this._catalog + "/review/delegate";

		var handler = this._handler(200, success, failure);

		this._post(url, data, handler);
	},

	review_revoke : function(data, success, failure) {

		var url = this._catalog + "/review/revoke";

		var handler = this._handler(200, success, failure);

		this._post(url, data, handler);
	},

	catalog_review_list : function(success, failure) {

		var url = this._catalog + "/review/read/list";

		var handler = this._handler(200, success, failure);

		this._get(url, handler);
	}
};


function Model (xhr) {
	this._xhr = xhr;
	this._search = [];
	this._review = [];
	this._last_search = "";
	this._catalog = {
		review : [],
		pcap : [],
		sqlite : [] };
	this._users = [];

	this.searchEvent = new Event(this);
	this.reviewEvent = new Event(this);
	this.catalogEvent = new Event(this);
}

Model.prototype = {

	create_review : function(id, title, rating, comment) {
		var obj = {
			id : id,
			title : title,
			rating : rating,
			comment : comment
		};

		var _this = this;
		var success = function() {
			_this._review.push(obj);
			_this.reviewEvent.notify({
				event: "create",
				data : obj
			});
		};
		this._xhr.create_review(obj, success);
	},

	update_review : function(id, title, rating, comment) {
		var obj = {
			id : id,
			title : title,
			rating : rating,
			comment : comment
		};

		var _this = this;
		var success = function() {
			_this.reviewEvent.notify({
				event : "update",
				data : obj
			});
		};
		this._xhr.update_review(obj, success);
	},

	remove_review : function(id, title) {
		var _this = this;
		var obj = {
			id : id,
			title : title
		};

		var success = function() {
			var i = 0, inx = -1;
			for (; i < _this._review.length; ++i) {
				if (_this._review[i].id === id) inx = i;
			}
			_this._review.splice(inx, 1);

			_this.reviewEvent.notify({
				event : "remove",
				data : obj
			});
		};
		this._xhr.remove_review(id, success);
	},

	read_review : function(id) {
		var _this = this;
		var success = function(response) {
			var obj = JSON.parse(response);
			_this._review.push(obj);
			_this.reviewEvent.notify({
				event : "read",
				data : obj
			});
		};
		this._xhr.read_review(id, success);
	},

	isReviewd : function(id) {
		var i= 0, inx = -1;
		for (; i < this._review.length; ++i) {
			if (this._review[i].id === id) inx = i;
		}
		return inx >= 0;
	},

	countReview : function() {
		return this._review.length;
	},

	//filter out movies already in reviewed movies
	//then notify with left ones
	search_results : function(title) {
		this._last_search = title;	
    this._search = [];

    var _this = this;
    var success = function(response) {
    	var results = JSON.parse(response);
    	var i = 0;
    	for (; i < results.length; ++i) {
    		var r = results[i];
    		if (!_this.isReviewd(r.id)) {
    			_this._search.push(r);
    		}
    	}

    	_this.searchEvent.notify({
    		event : "results",
    		data : _this._search
    	});
    };

    this._xhr.search_movie(title, success);
	},

	add_search_item : function(id, title) {

		var checkEq = function(tx, ty) {
			console.log(tx + "<>" + ty);
			console.log(tx.toLowerCase().indexOf(ty.toLowerCase()));
			return tx !== "" &&
			       ty !== "" &&
			       tx.toLowerCase().indexOf(ty.toLowerCase()) >= 0;
		};

		if (checkEq(title, this._last_search)) {
			var obj = {
				id : id,
				title : title
			};

			this._search.push(obj);
			this.searchEvent.notify({
				event : "create",
				data : obj
			});
		}
	},

	remove_search_item : function(id, title) {
		var inx = -1, i = 0;
		for (; i < this._search.length; ++i) {
			if (this._search[i].id === id) {
				inx = i;
			}
		}
		this._search.splice(inx, 1);

		this.searchEvent.notify({
			event : "remove",
			data : {id : id, title : title}
		});
	},

	count_search : function() {
		return this._search.length;
	},

	sync_catalog_review : function() {
		var _this = this;
		var success = function(response) {
			var remote = JSON.parse(response),
				  local = _this._catalog.review;
			var i = 0, id;
			
			for (; i < remote.length; ++i) {
				id = remote[i];
				if (local.indexOf(id) == -1) {
					var j = 0, inx = -1, data;

					for (; j < _this._review.length; ++j) {
						if (_this._review[j].id == id) inx = j;
					}

					if (inx == -1) data = {};
					else data = _this._review[inx]; 

					_this.catalogEvent.notify({
						source : "review",
						event : "create",
						data : data
					});
				}
			}

			i = 0;
			for (; i < local.length; ++i) {
				id = local[i];
				if (-1 == remote.indexOf(id)) {
					_this.catalogEvent.notify({
						source : "review",
						event : "remove",
						data : id
					});
				}
			}

			_this._catalog.review = remote;
		};
		this._xhr.sync_catalog_review(success);
	},

	update_catalog_users : function() {
		var _this = this;
		var success = function(response) {
			var users = JSON.parse(response);
			_this._users = users;
		};
		this._xhr.update_catalog_users(success);
	},

	upload_review : function(id) {
		var _this = this;
		var success = function(response) {
			var meta = JSON.parse(response),
			    obj = {
			      id : Utils.toCatalogId(id),
			      file_id : meta.file_id,
			    };

			_this.catalogEvent.notify({
				source : "review",
				event : "uploaded",
				data : obj,
			});
		};
		this._xhr.upload_review(id, success);
	},

	review_delegate : function(obj) {
		var _this = this;
		var success = function(){
			_this.catalogEvent.notify({
				source : "review",
				event : "delegated",
				data : obj,
			});
		};
		var data = {
			file_id : obj.file_id,
			user_id : obj.user_id
		};
		this._xhr.review_delegate(data, success);
	},

	review_revoke : function(obj) {
		var _this = this;
		var success = function(){
			_this.catalogEvent.notify({
				source : "review",
				event : "revoked",
				data : obj,
			});
		};
		var data = {
			file_id : obj.file_id,
			user_id : obj.user_id
		};
		this._xhr.review_revoke(data, success);
	},

	get_users : function() {
		return this._users;
	},

	init_catalog_review : function() {
		var _this = this;
		
		var success = function(response) {
			
			var reviews = JSON.parse(response);

			for (var id in reviews) {
				if (reviews.hasOwnProperty(id)) {
					var info = reviews[id];
					if (Object.keys(info).length === 0) {
						var success = function(response) {
							var review = JSON.parse(response);
							_this.catalogEvent.notify({
								source : "review",
								event : "create",
								data : review
							});
						};
						_this._xhr.read_review(id, success);
					} else {
						var success = function(response) {
							var review = JSON.parse(response);
							_this.catalogEvent.notify({
								source : "review",
								event : "read",
								data : {
									"review" : review,
									"info" : info
								}
							});
						};
						_this._xhr.read_review(id, success);
					}
				}
			}

			_this._catalog.review = Object.keys(reviews);
		};
		this._xhr.catalog_review_list(success);
	},

	init : function() {
		var _this = this;
		var success = function(response){
			var i = 0, lst = JSON.parse(response);
			for (; i < lst.length; ++i) {
				_this.read_review(lst[i]);
			}
			_this.update_catalog_users();
			//setTimeout(_this.init_catalog_review, 1500);
			_this.init_catalog_review();
		};
		this._xhr.list_reviews(success);
	}
};

function View(model, elements, templates) {
	this._model = model;
	this._elements = elements;
	this._templates = templates;

	this.buttonEvent = new Event(this);

	var _this = this;

	/* user operation, need broadcast/processing */

	this._elements.searchBtn.click(function(){
		var input = $("#search-input");
		var title = input.val();
		input.val("");

		if (title !== "" && title != _this._model._last_searchs) {
			_this.buttonEvent.notify({
				event : "search",
				data : title
			});		
		}
	});

	//submit button should have "for=`id'" attribute
	this._elements.submitBtn.click(function(){
		var id = $(this).attr("for");
		var obj = _this.read_info(id);
		
		var event = "";
		if (_this._model.isReviewd(id)) event = "update";
		else event = "create";

		_this.buttonEvent.notify({
			event : event,
			data : obj
		});
	});

	//side button should have "for = `id'" attribute
	//form container should have "for = `id'" attribute
	this._elements.sideBtn.click(function(){
		var btn = $(this);
		var id = btn.attr("for"),
		    cls = btn.attr("class");
		
		var event = "";
		if (cls.indexOf("create") >= 0 || cls.indexOf("update") >= 0) {
			$("li[for=" + id + "]").toggle();
		} else if (cls.indexOf("remove") >= 0) {
			event = "remove";
		} else {}

		if (event === "remove") {
			var title = $("#" + id).find("span.title").text();
			var obj = {
				id : id,
				title : title
			};

			_this.buttonEvent.notify({
				event : "remove",
				data : obj
			});	
		}
	});

	this._elements.catReviewDiv.find("button.sync").click(function(){
		//$(this).hide();
		_this.buttonEvent.notify({
			event : "catalog-review-sync",
			data : {}
		});
	});

	this._elements.encryptBtn.click(function(){
		var btn = $(this),
		    id = btn.attr("for");

		_this.buttonEvent.notify({
			event : "catalog-review-upload",
			data : Utils.ofCatalogId(id)
		});
	});
	/* notification from model, need rerender */

	this._model.reviewEvent.register(function(sender, arg){
		switch(arg.event) {
			case "create":
			case "read":
				_this.create_review(arg.data);
				//_this.enable_catalog_sync();
				break;
			case "remove":
				_this.remove_review(arg.data);
				break;
			case "update":
				break;
			default:
		}
	});

	this._model.searchEvent.register(function(sender, arg){
		switch(arg.event) {
			case "create":
				_this.create_search(arg.data);
				break;
			case "remove":
				_this.remove_search(arg.data);
				break;
			case "results":
				_this.add_results(arg.data);
				break;
			default:
		}
	});

	this._model.catalogEvent.register(function(sender, arg){
		switch(arg.source) {
			case "review":
				switch(arg.event) {
					case "remove":
						_this.remove_catalog_review(arg.data);
						break;
					case "create":
						_this.create_catalog_review(arg.data);
						break;
					case 	"read":
						_this.read_catalog_review(arg.data);
						break;
					case "uploaded":
						_this.catalog_review_uploaded(arg.data);
						break;
					case "delegated":
						_this.catalog_review_delegated(arg.data);
						break;
					case "revoked":
						_this.catalog_review_revoked(arg.data);
						break;
					default:
						console.log("not matched catalog review event" + arg.event);
				}
				break;
			default:
				console.log("not matched catalog event source: " + arg.source);
		}
	});
}


View.prototype = {
	new_li : function(id, title) {
		var li = $(this._templates.find("li.title").clone(true));
		li.attr("id", id);
		li.find("span.title").text(title);
		return li;
	},

	new_form : function(id) {
		var ctn = $(this._templates.find("li.form-ctn").clone(true));
		ctn.attr("for", id);
		ctn.find("button.submit").attr("for", id);
		return ctn;
	},

	updateReviewCount : function() {
		var num = this._model.countReview();
		this._elements.reviewBadge.text(num);
	},

	create_review : function(review) {
		var li = this.new_li(review.id, review.title),
			  ctn_li = this.new_form(review.id),
				ubtn = this._templates.find("button.update").clone(true),
		    rbtn = this._templates.find("button.remove").clone(true);
		 
		 ubtn.attr("for", review.id);
		 rbtn.attr("for", review.id);
		 li.append(ubtn, rbtn);

		 ctn_li.find("select").val(review.rating);
		 ctn_li.find("textarea").val(review.comment);

		 this._elements.reviewUl.append(li, ctn_li);
		 this.updateReviewCount();
	},

	remove_review : function(review) {
		var li = this._elements.reviewUl.find("#" + review.id),
		    ctn_li = this._elements.reviewUl.find("li[for=" + review.id + "]");

		li.remove();
		ctn_li.remove();
		this.updateReviewCount();
	},

	updateSearchCount : function() {
		var num = this._model.count_search();
		this._elements.searchBadge.text(num);
	},

	create_search : function(search) {
		var li = this.new_li(search.id, search.title),
		    ctn_li = this.new_form(search.id),
		    cbtn = this._templates.find("button.create").clone(true);

		cbtn.attr("for", search.id);
		li.append(cbtn);

		this._elements.searchUl.append(li);
		this._elements.searchUl.append(ctn_li);
		this.updateSearchCount();
	},

	remove_search : function(search) {
		var li = this._elements.searchUl.find("#" + search.id),
		    ctn_li = this._elements.searchUl.find("li[for=" + search.id + "]");

		li.remove();
		ctn_li.remove();
		this.updateSearchCount();
	},

	add_results : function(results) {
		this._elements.searchUl.empty();

		var i = 0;
		for (; i < results.length; ++i) {
			this.create_search(results[i]);
		}
	},

	read_info : function(id) {
		var ul;
		if (this._model.isReviewd(id)) ul = this._elements.reviewUl;
		else ul = this._elements.searchUl;

		var title = ul.find("#" + id + " span.title").text();
		
		var form = ul.find("li[for=" + id + "]");
		var rating = form.find("select").val(),
		    comment = form.find("textarea").val();

		return {
			id : id,
			title : title,
			rating : rating,
			comment : comment
		};
	},

	enable_catalog_sync : function() {
		this._elements.catReviewDiv.find("button.sync").show();
	},

	create_catalog_review : function(review) {
		var _this = this;
		var id = Utils.toCatalogId(review.id),
		    li = this.new_li(id, review.title),
		    ctn = this._templates.find("li.catalog-item-ctn").clone(true),
		    ebtn = this._templates.find("button.upload").clone(true);

		var i = 0,
			  users = this._model.get_users(),
		    dselect = ctn.find("select.delegate");

		ebtn.attr("for", id);
		li.append(ebtn);
		ctn.attr("for", id);
		for(; i < users.length; ++i) {
			var u = users[i];
			var option = $("<option></option>").attr("value", u).text(u);
			dselect.append(option);
		}
		ctn.find("button.apply-delegate").attr("for", id).click(function(){
			var btn = $(this),
					id = btn.attr("for");

			var file_id = _this._elements.catReviewDiv.find("#" + id).attr("file-id"),
			    user_id = _this._elements.catReviewDiv.find("li[for=" + id + "] select.delegate").val();
			
			var obj = {
				id : id,
				source : "review",
				file_id : file_id,
				user_id : user_id
			};

			if (user_id !== "0") {
				_this.buttonEvent.notify({
				event : "catalog-review-delegate",
				data : obj,
			});}
		});
		ctn.find("button.apply-revoke").attr("for", id).click(function(){
			var btn = $(this),
					id = btn.attr("for");

			var file_id = _this._elements.catReviewDiv.find("#" + id).attr("file-id"),
			    user_id = _this._elements.catReviewDiv.find("li[for=" + id + "] select.revoke").val();
			
			var obj = {
				id : id,
				source : "review",
				file_id : file_id,
				user_id : user_id
			};

			if (user_id !== "0") {
				_this.buttonEvent.notify({
				event : "catalog-review-revoke",
				data : obj,
			});}
		});

		this._elements.catReviewDiv.find("ul").append(li, ctn);
	},

	catalog_review_uploaded : function(obj) {
		var li = this._elements.catReviewDiv.find("#" + obj.id),
		    okbtn = this._templates.find("button.encrypted").clone(true),
		    ubtn = this._templates.find("button.update").clone(true);
		
		li.attr("file-id", obj.file_id);
		li.find("button.upload").remove();
		okbtn.attr("for", obj.id);
		ubtn.attr("for", obj.id);
		li.append(ubtn);
		li.append(okbtn);
	},

	catalog_review_delegated : function(obj) {
		var id = obj.id,
		    ctn = this._elements.catReviewDiv.find("li[for=" + id + "]"),
		    option = ctn.find("select.delegate option[value=" + obj.user_id + "]"),
		    rselect = ctn.find("select.revoke");

		option.detach();
		rselect.append(option);
	},

	read_catalog_review : function(data) {
		this.create_catalog_review(data.review);

		var id = Utils.toCatalogId(data.review.id);
		var obj = {
			id : id,
			file_id : data.info.file_id
		};
		this.catalog_review_uploaded(obj);

		var i = 0;
		for (; i < data.info.delegations.length; ++i) {
			var user_id = data.info.delegations[i];
			obj = {
				id : id,
				user_id : user_id
			};

			this.catalog_review_delegated(obj);
		}
	},

	catalog_review_revoked : function(obj) {
		var id = obj.id,
		    ctn = this._elements.catReviewDiv.find("li[for=" + id + "]"),
		    option = ctn.find("select.revoke option[value=" + obj.user_id + "]"),
		    dselect = ctn.find("select.delegate");

		option.detach();
		dselect.append(option);
	}	
};


function Controller(model, view) {
	this._model = model;
	this._view = view;

	var _this = this;

	this._view.buttonEvent.register(function(sender, arg){
		switch(arg.event) {
			case "create":
				_this.create_review(arg.data);
				break;
			case "update":
				_this.update_review(arg.data);
				break;
			case "remove":
				_this.remove_review(arg.data);
				break;
			case "search":
				_this._model.search_results(arg.data);
				break;
			case "catalog-review-sync":
				_this.catalog_review_sync();
				break;
			case "catalog-review-upload":
				_this.catalog_review_upload(arg.data);
				break;
			case "catalog-review-delegate":
				_this.catalog_review_delegate(arg.data);
				break;
			case "catalog-review-revoke":
				_this.catalog_review_revoke(arg.data);
				break;
			default:
		}
	});
}

Controller.prototype = {
	create_review : function(obj) {
		this._model.create_review(obj.id, obj.title, obj.rating, obj.comment);
		this._model.remove_search_item(obj.id, obj.title);
	},

	update_review : function(obj) {
		this._model.update_review(obj.id, obj.title, obj.rating, obj.comment);
	},

	remove_review : function(obj) {
		this._model.remove_review(obj.id, obj.title);
		this._model.add_search_item(obj.id, obj.title);
	},

	catalog_review_sync : function() {
		//this._model.update_catalog_users();
		this._model.sync_catalog_review();
	},

	catalog_review_upload : function(obj) {
		this._model.upload_review(obj);
	},

	catalog_review_delegate : function(obj) {
		this._model.review_delegate(obj);
	},

	catalog_review_revoke : function(obj) {
		this._model.review_revoke(obj);
	},
};


(function(){
	var xhr = new XHR(),
		  model = new Model(xhr),
		  view = new View(model, {
		  	searchUl : $("#search-results > ul"),
		  	reviewUl : $("#reviewed > ul"),
		  	catReviewDiv : $("#catalog-review"),
		  	searchBadge : $("#search-results span.badge"),
		  	reviewBadge : $("#reviewed span.badge"),
		  	searchBtn : $("#search"),
		  	submitBtn : $("button.submit"),
		  	sideBtn : $("button.side"),
		  	encryptBtn : $("button.upload")
		  }, $("#templates")),
		  controller = new Controller(model, view);

	model.init();
})();