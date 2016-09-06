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
    this._root = "";
    this._review = "/review";
    this._catalog = "/catalog";
    this._gatekeeper = "/gatekeeper";

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

    /* review */

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


    /* catalog */

    sync_catalog_review: function(success, failure) {

	var url = this._catalog + "/review/read/list";

	var handler = this._handler(200, success, failure);

	this._post(url, {}, handler);
    },

    update_catalog_users : function(success, failure) {

	var url = this._catalog + "/users";

	var handler = this._handler(200, success, failure);

	this._get(url, handler);
    },

    read_review_meta: function(id, success, failure) {

        var url = this._catalog + "/review/read/meta/" + id;

        var handler = this._handler(200, success, failure);

        this._get(url, handler);
    },

    upload_review : function(data, success, failure) {

	var url = this._catalog + "/review/upload";

	var handler = this._handler(200, success, failure);

	this._post(url, data, handler);
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
    },


    /* gatekeeper */

    approve_access : function(data, success, failure) {

        var id = data.id;
        var domain = data.domain;
        var url = this._gatekeeper + "/op/approve/" + id + "/" + domain;

        var handler = this._handler(200, success, failure);

        this._post(url, {}, handler);
    },

    reject_access : function(data, success, failure) {

        var id = data.id;
        var domain = data.domain;
        var url = this._gatekeeper + "/op/reject/" + id + "/" + domain;

        var handler = this._handler(200, success, failure);

        this._post(url, {}, handler);
    },

    remove_item  : function(data, success, failure) {

        var path = data.path;
        var url = this._gatekeeper + "/op/remove/" + path;

        var handler = this._handler(200, success, failure);

        this._post(url, {}, handler);
    },

    list_items : function(data, success, failure) {

        var path = data.path;
        var url = this._gatekeeper + "/op/list/" + path;

        var handler = this._handler(200, success, failure);

        this._post(url, {}, handler);
    },
};


function Model (xhr) {
    this._xhr = xhr;
    this._search = [];
    this._review = [];
    this._last_search = "";
    this._catalog = {
	review : []
    };
    this._gatekeeper = {
        approved : [],
        rejected : [],
        pending  : []
    };
    this._users = [];

    this.searchEvent = new Event(this);
    this.reviewEvent = new Event(this);
    this.catalogEvent = new Event(this);
    this.gateKeeperEvent = new Event(this);
}

Model.prototype = {

    _remove_item : function(arr, check_fn) {

        var i = arr.length - 1, inx = -1;
        for (; i >= 0; i--) {
            if (check_fn(arr[i])) {
                inx = i;
            }
        }

        if (inx === -1) return {};
        else {
            var rt = arr[inx];
            arr.splice(inx, 1);
            return rt;
        }
    },

    approve_access : function(id, domain) {
        var obj = {
            id : id,
            domain : domain
        };

        var _this = this;
        var success = function() {
            var check_fn = function(arg) {
                return arg.id === id && arg.domain === domain;
            };

            var item = _this._remove_item(_this._gatekeeper.pending, check_fn);
            _this._gatekeeper.approved.push(item);

            _this.gateKeeperEvent.notify({
                event : "approve",
                data  : obj
            });
        }
        this._xhr.approve_access(obj, success);
    },

    reject_access : function(id, domain) {
        var obj = {
            id : id,
            domain : domain
        };

        var _this = this;
        var success = function() {
            var check_fn = function(arg) {
                return arg.id === id && arg.domain === domain;
            };

            var item = _this._remove_item(_this._gatekeeper.pending, check_fn);
            _this._gatekeeper.rejected.push(item);

            _this.gateKeeperEvent.notify({
                event : "reject",
                data  : obj
            });
        }
        this._xhr.reject_access(obj, success);
    },

    remove_gk_item : function(category, id, domain) {
        var obj = {
            path : category + "/" + id + "/" + domain
        };

        var _this = this;
        var success = function() {
            var arr;
            if (category === "approved") arr = _this._gatekeeper.approved;
            else if (category === "rejected") arr = _this._gatekeeper.rejected;
            else arr = _this._gatekeeper.pending;

            var check_fn = function(arg) {
                return arg.id === id && arg.domain === domain;
            };
            _this._remove_item(arr, check_fn);

            _this.gateKeeperEvent.notify({
                event : "remove",
                data  : {
                    category : category,
                    id       : id,
                    domain   : domain
                }
            });
        };

        this._xhr.remove_item(obj, success);
    },

    populate_gk_category : function(category) {
        var arr;
        if (category === "approved") arr = this._gatekeeper.approved;
        else if (category === "rejected") arr = this._gatekeeper.rejected;
        else arr = this._gatekeeper.pending;

        //bug to fix!!! remove the list in the view but not in the model
        while(arr.length !== 0) {
            var tmp = arr.pop();
            this.remove_gk_item(category, tmp.id, tmp.domain);
        }

        var _this = this;
        var obj = {path : category};
        var success = function(response) {
            var id_arr = JSON.parse(response);
            console.log("populate " + category + ": " + id_arr);
            for (var i = 0; i < id_arr.length; i++) {
                var id = id_arr[i];
                var obj_tmp = {path : category + "/" + id};
                var success_tmp = function(response_tmp) {
                    var domain_arr = JSON.parse(response_tmp);
                    console.log("domains: " + response_tmp);
                    for (var j = 0; j < domain_arr.length; j++) {
                        var domain = domain_arr[j];
                        arr.push({id : id, domain : domain});
                        _this.gateKeeperEvent.notify({
                            event : "populate",
                            data  : {
                                category : category,
                                id       : id,
                                domain   : domain
                            }
                        });
                    }
                };
                _this._xhr.list_items(obj_tmp, success_tmp);
            }
        };
        this._xhr.list_items(obj, success);
    },

    list_reviews : function() {
        var _this = this;
        var success = function(response){
	    var i = 0, lst = JSON.parse(response);
            for (; i < lst.length; ++i) {
		_this.read_review(lst[i]);
	    }
        };
        this._xhr.list_reviews(success);
    },

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
	    var remote = JSON.parse(response);
            console.log("sync_catalog_review: " + remote);

	    var local = _this._catalog.review;
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
        var review_id = Utils.ofCatalogId(id);
        var success = function(response) {
            console.log("got meta: " + response);
            var meta = JSON.parse(response);

            var success = function(){
                var obj = {
		    id : id,
		    file_id : meta.file_id
	        };

                console.log("to notify uploaded: " + obj);

	        _this.catalogEvent.notify({
		    source : "review",
		    event : "uploaded",
		    data : obj,
	        });
            }
            _this._xhr.upload_review(meta, success);
        };

        this._xhr.read_review_meta(review_id, success);
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
	//this._xhr.list_reviews(success);
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


    this._elements.listReview.click(function(){
        _this.buttonEvent.notify({
            event: "list-review"
        });
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


    this._elements.populateBtn.click(function() {
        var btn = $(this);
        var cls = btn.attr("class");

        var category = "";
        if (cls.indexOf("approved") >= 0) category = "approved";
        else if (cls.indexOf("rejected") >= 0) category = "rejected";
        else category = "pending";

        if (category !== "") {
            console.log("populate: " + category);
            _this.buttonEvent.notify({
                event : "gatekeeper-populate-category",
                data  : {category : category}
            });
        }
    });


    this._elements.gkBtn.click(function() {
        var btn = $(this);
        var info = btn.attr("for"),
            cls = btn.attr("class");

        var category, id, domain, pos;
        pos = info.indexOf(" ");
        category = info.substring(0, pos);
        info = info.substring(pos + 1);
        pos = info.indexOf(" ");
        id = info.substring(0, pos);
        domain = info.substring(pos + 1);

        var event;
        if (cls.indexOf("approve") >= 0) event = "gatekeeper-approve-access";
        else if (cls.indexOf("reject") >= 0) event = "gatekeeper-reject-access";
        else event = "gatekeeper-remove-item";

        if (event === "gatekeeper-remove-item") {
            _this.buttonEvent.notify({
                event : event,
                data  : {
                    category : category,
                    id       : id,
                    domain   : domain
                }
            });
        } else {
            _this.buttonEvent.notify({
                event : event,
                data  : {
                    id     : id,
                    domain : domain
                }
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
	    data : id
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
	    case "read":
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

    this._model.gateKeeperEvent.register(function(sender, arg) {
        switch(arg.event) {
        case "populate":
            _this.create_gatekeeper_item(arg.data);
            break;
        case "remove":
            _this.remove_gatekeeper_item(arg.data);
            break;
        case "reject":
            _this.remove_gatekeeper_item({
                category : "pending",
                id       : arg.data.id,
                domain   : arg.data.domain});
            _this.create_gatekeeper_item({
                category : "rejected",
                id       : arg.data.id,
                domain   : arg.data.domain
            });
            break;
        case "approve":
            _this.remove_gatekeeper_item({
                category : "pending",
                id       : arg.data.id,
                domain   : arg.data.domain});
            _this.create_gatekeeper_item({
                category : "approved",
                id       : arg.data.id,
                domain   : arg.data.domain
            });
            break;
        default:
            console.log("unknown gateKeeperEvent: " + arg.event);
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
    },

    create_gatekeeper_item : function(obj) {
        var for_attr = obj.category + " " + obj.id + " " + obj.domain;
        var title = obj.id.substr(0, 6) + "    " + obj.domain;
        var li = this.new_li(obj.id, title);
        li.attr("for", for_attr);

        if(obj.category === "pending") {
            var abtn = this._templates.find("button.approve").clone(true);
            abtn.attr("for", for_attr);
            var rbtn = this._templates.find("button.reject").clone(true);
            rbtn.attr("for", for_attr);
            li.append(abtn, rbtn);
        }

        var rmbtn = this._templates.find("button.gk-remove").clone(true);
        rmbtn.attr("for", for_attr);
        li.append(rmbtn);

        var ul = this._elements.gateKeeperDiv.find("ul[for=" + obj.category + "]");
        ul.append(li);
    },

    remove_gatekeeper_item : function(obj) {
        var for_attr = obj.category + " " + obj.id + " " + obj.domain;
        var li = this._elements.gateKeeperDiv.find("li[for='" + for_attr + "']");

        li.remove();
    }
};


function Controller(model, view) {
    this._model = model;
    this._view = view;

    var _this = this;

    this._view.buttonEvent.register(function(sender, arg){
	switch(arg.event) {
        case "list-review":
            _this.list_reviews();
            break;
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
        case "gatekeeper-populate-category":
            _this.gk_populate_category(arg.data);
            break;
        case "gatekeeper-approve-access":
            _this.gk_approve_access(arg.data);
            break;
        case "gatekeeper-reject-access":
            _this.gk_reject_access(arg.data);
            break;
        case "gatekeeper-remove-item":
            _this.gk_remove_item(arg.data);
            break;
	default:
            console.log("unknown buttonEvent: " + arg.event);
	}
    });
}

Controller.prototype = {
    gk_populate_category : function(obj) {
        console.log("controller populate category");
        this._model.populate_gk_category(obj.category);
    },

    gk_approve_access : function(obj) {
        this._model.approve_access(obj.id, obj.domain);
    },

    gk_reject_access : function(obj) {
        this._model.reject_access(obj.id, obj.domain);
    },

    gk_remove_item : function(obj) {
        this._model.remove_gk_item(obj.category, obj.id, obj.domain);
    },

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

    list_reviews : function(obj) {
        this._model.list_reviews();
    },

    catalog_review_sync : function() {
	this._model.update_catalog_users();
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
	searchBtn : $("#search"),

	searchUl : $("#search-results > ul"),
	searchBadge : $("#search-results span.badge"),
	reviewUl : $("#reviewed > ul"),
	reviewBadge : $("#reviewed span.badge"),
        listReview: $("button.list-review"),

	submitBtn : $("button.submit"),
	sideBtn : $("button.side"),
	encryptBtn : $("button.upload"),

	catReviewDiv : $("#catalog-review"),

        populateBtn : $("button.populate"),
        gkBtn : $("button.gatekeeper"),
        gateKeeperDiv : $("#gatekeeper")
    }, $("#templates")),
    controller = new Controller(model, view);

    model.init();
})();
